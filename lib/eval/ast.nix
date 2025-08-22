{ lib, collective-lib, nix-reflect, ... }:

with collective-lib.typed;
with nix-reflect.eval.monad;

let
  inherit (nix-reflect.parser) AST N isAST parse;
  do = Eval.do;
in rec {
  # Module callable as eval.ast
  __functor = self: self.evalAST;

  /*
  Parse the expression in the Eval monad and drop the state from the result.

  Exposed as eval.eval.ast (and eval.eval) in default.nix for use as just "eval"
 
  runAST :: (string | AST) -> Either EvalError {a :: a, s :: EvalState} */
  runAST = runAST_ false;
  runAST' = runAST_ true;
  runAST_ = strict: expr:
    (do
      (whileV 1 "evaluating string or AST node: ${_p_ expr}")
      (evalM strict expr))
    .runEmpty {};

  /*
  evalAST :: (string | AST) -> Either EvalError a */
  evalAST = evalAST_ false;
  evalAST' = evalAST_ true;
  evalAST_ = strict: expr:
    (do
      (whileV 1 "evaluating string or AST node: ${_p_ expr}")
      (evalM strict expr))
    .runEmpty_ {};

  /*
  evalM :: (string | AST) -> bool -> Eval a */
  evalM = strict: expr:
    let parsed = parse expr;
    in with (log.v 2).call "evalM" expr ___;
    {_, ...}: _.do
      (whileV 1 "evaluating parsed AST node: ${try (toString parsed) (_: "<parse failed>")}")
      (set initEvalState)
      {result_ = evalNodeM parsed;}
      {result = {result_, _}: if strict then _.bind (forceDeeply result_) else _.bind (force result_);}
      ({_, result, ...}: _.pure (return (maybeConvertLambda result)));

  maybeConvertLambda = x:
    if isEvaluatedLambda x then x.asLambda
    else if x ? __functor then x // {__functor = maybeConvertLambda x.__functor; }
    else x;

  /* Main monadic eval entrypoint.
  Evaluates a node down to its WHNF value.
  evalNodeM :: AST -> Eval a */
  evalNodeM = node:
    with (log.v 3).call "evalNodeM" (toString node) ___;
    do
      (whileV 2 "evaluating AST node: ${toString node}")
      (if is EvalError node then liftEither node else pure unit)
      (guard (is AST node) (RuntimeError ''
        evalNodeM: Expected AST node or NodeThunk, got ${_ph_ node}
      ''))
      {res = switch node.nodeType {
        int = evalLiteral node;
        float = evalLiteral node;
        string = evalLiteral node;
        stringPieces = evalStringPieces node;
        path = evalPath node;
        anglePath = evalAnglePath node;
        list = evalList node;
        attrs = evalAttrs node;
        identifier = evalIdentifier node;
        binaryOp = evalBinaryOp node;
        unaryOp = evalUnaryOp node;
        conditional = evalConditional node;
        lambda = evalLambda node;
        application = evalApplication node;
        letIn = evalLetIn node;
        assignment = evalAssignment node;
        "with" = evalWith node;
        "assert" = evalAssert node;
        "abort" = evalAbort node;
        "throw" = evalThrow node;
        "import" = evalImport node;
        "inherit" = evalInherit node;
       };}
       ({_, res}: _.pure (return res));

  deeplyEvalNodeM = node: {_, ...}:
    _.do
      (whileV 2 "deeply evaluating AST node: ${toString node}")
      (forceDeeplyM (evalNodeM node));

  forceEvalNodeM = node: {_, ...}:
    _.do
      (whileV 2 "evaluating AST node to WHNF: ${toString node}")
      (forceM (evalNodeM node));

  forceM = m: {_, ...}:
    _.do
      (whileV 2 "forcing a monadic value")
      {unforced = m;}
      ({_, unforced}: _.bind (force unforced));

  forceDeeplyM = m: {_, ...}:
    _.do
      (whileV 2 "deeply forcing a monadic value")
      {unforced = m;}
      ({_, unforced}: _.bind (forceDeeply unforced));

  force = x: {_, ...}: _.do
    (while "forcing shallowly")
    (if isNodeThunk x then x.force
     else pure x);

  isNodeThunk = x: x ? __isNodeThunk;

  mkNodeThunk = _: node: lib.fix (self: {
    __isNodeThunk = true;
    inherit (node) nodeType;
    __toString = self: "<CODE>";
    # Force the thunk down to WHNF.
    force =
      (_.do
        (while "forcing '${self.nodeType}' thunk")
        # Handle nested thunks
        {forced = force node;}
        ({forced, _}: _.bind (forceEvalNodeM forced))
      ).runClosureM;
  });

  NodeThunk = node: {_, ...}: _.do
    (while "constructing '${node.nodeType}' NodeThunk")
    ({_}: _.pure (mkNodeThunk _ node));

  forceDeeply = x: {_, ...}: _.do
    (while "forcing deeply")
    ({_}:
      if isEvaluatedLambda x then _.pure x
      else if isAST x then _.pure x
      else if isNodeThunk x then _.do
        {forced = x.force;}
        ({_, forced}: _.bind (forceDeeply forced))
      else if isList x then _.traverse forceDeeply x
      else if isAttrs x then _.do
        {forcedSolos =
          traverse
            (s: {_, ...}: _.do
              {forced = forceDeeply (soloValue s);}
              ({forced, _}: _.pure { ${soloName s} = forced;}))
            (solos x);}
        ({forcedSolos, _}: _.pure (mergeSolos forcedSolos))
      else _.pure x);

  # Evaluate a literal value (int, float, string, etc.)
  # evalLiteral :: AST -> Eval a
  evalLiteral = node: {_, ...}:
    _.do
      (while "evaluating 'literal' node")
      (pure node.value);

  # Evaluate a name (identifier or string)
  # If identifier, get the name itself, not evaluate it.
  # This can then be used on LHS of assignments.
  # Could also be a string or an interpolation.
  # Always used in places where we need to reference the name, so must
  # force its computation at least to WHNF.
  identifierName = node: {_, ...}:
    _.do
      (while "evaluating a name")
      {name = 
        if isString node then pure node
        else if node.nodeType == "identifier" then pure node.name
        else forceEvalNodeM node;}
      ({_, name}: _.do
        (guard (lib.isString name) (RuntimeError ''
          Expected string identifier name, got ${lib.typeOf name}
        ''))
        (pure name));

  # Evaluate an identifier lookup
  # evalIdentifier :: Scope -> AST -> Eval a
  evalIdentifier = node: {_, ...}:
    _.do
      (while "evaluating 'identifier' node")
      {scope = getScope;}
      ({_, scope}: _.do
        (guard (hasAttr node.name scope) (UnknownIdentifierError ''
          Undefined identifier '${node.name}' in current scope:
          ${_pd_ 1 scope}
        ''))
        (evalScopeValue node.name scope.${node.name}));

  # Evaluate a value retrieved from the scope.
  # Handles:
  # - AST Nodes: lazily stored scope-free AST nodes to be evaluated in the current scope
  # - Thunk Nodes: stored by any non-strict recursive context i.e. let bindings, rec-attrs (temporarily), with blocks
  # - Monadic values: any monadic action, bound to current scope
  # - Standard Nix values
  evalScopeValue = name: value: {_, ...}:
    _.do
      (while "evaluating value from scope")
      ({_}: 
        if isNodeThunk value then _.do
          (while "evaluating NodeThunk from scope")
          # Ensure the forced value replaces the thunk for all future computations
          # TODO: Cleaner with a separate thunk store
          {forced = force value;}
          ({_, forced}: _.do
            (appendScope { ${name} = forced; })
            (pure forced))

        else if isAST value then _.do
          (while "evaluating AST node from scope")
          # Evaluate once to WHNF, capturing the state but not deeply evaluating
          {evaluated = evalNodeM value;}
          ({_, evaluated}: _.do
            (appendScope { ${node.name} = evaluated; })
            (pure evaluated))

          # Any monadic state should be bound to
          else if isMonadOf Eval value then _.do
            (while "evaluating Eval value from scope")
            ({_}: _.bind value)

          # Or just return strict values from the store.
          else _.do
            (while "evaluating ${lib.typeOf value} from scope")
            (pure value));

  # Evaluate a list of AST nodes
  # evalList :: AST -> Eval [a]
  evalList = node: {_, ...}:
    _.do
      (while "evaluating 'list' node")
      (traverse NodeThunk node.elements);

  # Evaluate an assignment (name-value pair)
  # evalAssignment :: AST -> Eval [{name, value}]  
  evalAssignment = node: {_, ...}:
    _.do
      (while "evaluating 'assignment' node")
      {name = identifierName node.lhs;}
      #{value = evalNodeM node.rhs;}
      {value = NodeThunk node.rhs;}
      ({_, name, value}: _.pure [{ inherit name value; }]);

  # Evaluate an attribute from a source
  # evalAttrFrom :: Set -> AST -> Eval {name, value}  
  evalAttrFrom = from: attr: {_, ...}:
    _.do
      (while "evaluating 'attr' node by name")
      {name = identifierName attr;}
      ({_, name}: _.do
        (guard (hasAttr name from) (MissingAttributeError ''
          No attribute '${name}' found when inheriting from:
          ${_ph_ from}
        ''))
        (pure { inherit name; value = from.${name}; }));

  # Evaluate an inherit expression, possibly with a source
  # evalInheritSourceAttrs :: AST -> Eval Set
  evalInheritSourceAttrs = node: {_, ...}:
    # Either resolve inherits from the main scope (which may contan thunks)
    if node == null then _.bind getScope
    # Or evaluate down to WHNF, which must be an attrset
    else _.do
      {source = forceEvalNodeM node;}
      ({source, _}: _.do
        (guard (lib.isAttrs source) (TypeError ''
          Cannot inherit from non-attrset of type ${lib.typeOf source}:
            ${_ph_ source}
        ''))
        (pure source));

  # Evaluate an inherit expression, possibly with a source
  # evalInherit :: AST -> Eval [{name, value}]
  evalInherit = node: {_, ...}:
    _.do
      (while "evaluating 'inherit' node")
      {source = evalInheritSourceAttrs node.from;}
      ({_, source}: _.traverse (evalAttrFrom source) node.attrs);

  # Evaluate a list of inheritance or assignment expressions
  # evalBindingList :: AST -> Eval set
  evalBindingList = bindings: {_, ...}:
    _.do
      (while "evaluating 'bindings' node-list")
      # Have to force here to get the list of name/value pairs
      {attrsList = traverse forceEvalNodeM bindings;}
      {attrs = {_, attrsList}: _.pure (concatLists attrsList);}
      ({_, attrs}: _.guard (all (attr: (hasAttr "name" attr) && (hasAttr "value" attr)) attrs) (RuntimeError ''
        Recursive binding list evaluation produced invalid name/value pairs:
          bindings: ${_ph_ bindings}
          attrs: ${_ph_ attrs}
      ''))
      ({_, attrs, ...}: _.pure (listToAttrs attrs));

  # Evaluate an attribute set
  # evalAttrs :: AST -> Eval AttrSet
  evalAttrs = node: {_, ...}:
    _.do
      (while "evaluating 'attrs' node")
      (if node.isRec 
      then saveScope (evalRecBindingList node.bindings)
      else evalBindingList node.bindings);

  # Evaluate a binding without failing on missing names.
  # addRecBindingToScope :: AST -> Eval [{name, value}]
  addRecBindingToScope = binding: {_, ...}:
    _.do
      (while "evaluating 'binding' node for recursive bindings")
      # Need to force to WHNF here as we require the name/value list representation
      {attrsList = forceEvalNodeM binding;}
      ({attrsList, _}:
        let attrs = listToAttrs attrsList;
        in _.do
          # We store key-thunk pairs back into the store
          (appendScope attrs)
          # And also return key-thunk pairs for usage as attrsets or let bindings
          (pure attrs));

  # Create a forward reference for an AST binding before evaluating a full attrset
  storeRecAssignmentNode = node: {_, ...}:
    _.do
      {k = identifierName node.lhs;}
      # Write an action that will run once we have a complete set of recursive bindings.
      ({_, k}: _.appendScope { ${k} = node.rhs; });

  # Create a forward reference for an AST binding before evaluating a full attrset
  storeRecAssignmentAction = node: {_, ...}:
    _.do
      {k = identifierName node.lhs;}
      {v = NodeThunk node.rhs;}
      ({_, k, v}: _.appendScope { ${k} = v; });

  # Create a forward reference for an AST inherit statement before evaluating a full attrset
  storeRecInheritNodes = node:
    traverse
      (attr: {_}: _.do
        # Only evaluate the source to WHNF; 
        {source = evalInheritSourceAttrs node.from;}
        # TODO: disallow dynamic attributes
        {k = identifierName attr.name;}
        ({source, k, _}: _.appendScope {
          ${k} = source.${k};
        }))
      node.attrs;
        #appendScope{
        ## Write an action that will run once we have a complete set of recursive bindings.
        ## Covers the case of { a = ...; inherit (a) ...; } where the source can be recursive
        ## as only names and strings are permitted.
        ## TODO: wasteful since every attr recomputes the source.
        ## Needs a separate Thunk store.
        #${k} = Eval.do
        #  # Only evaluate the source to WHNF; 
        #  {source = evalInheritSourceAttrs node.from;}
        #  # TODO: disallow dynamic attributes
        #  {k = identifierName attr.name;}
        #  ({_, source, k}: _.pure source.${k});
      #})
      #node.attrs;

  storeRecBindingNodes = bindings: {_, ...}:
    let assignmentNodes = filter (b: b.nodeType == "assignment") bindings;
        inheritNodes = filter (b: b.nodeType == "inherit") bindings;
    in _.do
      # First store the ASTs so that at least we have working if slow scope for all assignments
      (traverse storeRecAssignmentNode assignmentNodes)
      # rec-inherits could refer to any of the assigned nodes
      (traverse storeRecInheritNode inheritNodes)
      # Then the Thunks, with the AST as fallback on first pass
      (traverse storeRecAssignmentAction assignmentNodes);

  # Evaluate a list of recursive bindings
  # These are added to the scope; it is up to the usage sites (i.e. attrSets, letIn, with) to
  # saveScope appropriately. This is to enable nesting.
  evalRecBindingList = bindings: {_}:
    _.do
      # Pre-populate the store with forward references for all assignments and inherits
      (storeRecBindingNodes bindings)
      # This also adds to the scope as it goes, so later evaluations should see the
      # non-AST eval'd versions as Thunks in the store.
      {attrsSets = traverse addRecBindingToScope bindings;}
      # Merge these, letting any errors propagate. Any missing identifier here is a true error.
      ({attrsSets, _}: _.pure (mergeAttrsList attrsSets));

  # Type-check binary operation operands
  # checkBinaryOpTypes :: String -> [TypeSet] -> a -> a -> Eval Unit
  checkBinaryOpTypes = op: compatibleTypeSets: l: r: result:
    if any id 
      (map 
        (typeSet: elem (lib.typeOf l) typeSet && elem (lib.typeOf r) typeSet)
        compatibleTypeSets)
    then Eval.pure result
    else Eval.throws (TypeError (''Incorrect types for binary operator ${op}:

      ${_ph_ l} and ${_ph_ r}
      
      (expected one of ${_l_ compatibleTypeSets})
    ''));

  guardOneBinaryOp = op: compatibleTypeSets: l: r: {_}:
    _.do
      (while "checking compatible types to ${op} op: ${_l_ compatibleTypeSets}")
      (guard 
        (any id 
          (map 
            (typeSet: elem (lib.typeOf l) typeSet && elem (lib.typeOf r) typeSet)
            compatibleTypeSets))
        (TypeError ''Incorrect types for binary operator ${op}:

          ${_ph_ l} and ${_ph_ r}
          
          (expected one of ${ll_ compatibleTypeSets})
        ''));

  guardBinaryOp = l: op: r: {_}:
    _.do
      (while "guarding argument types to ${op} op")
      (switch op {
        "+" = guardOneBinaryOp "+" [["int" "float"] ["string" "path"]] l r;
        "-" = guardOneBinaryOp "-" [["int" "float"]] l r;
        "*" = guardOneBinaryOp "*" [["int" "float"]] l r;
        "/" = guardOneBinaryOp "/" [["int" "float"]] l r;
        "++" = guardOneBinaryOp "++" [["list"]] l r;
        "//" = guardOneBinaryOp "//" [["set"]] l r;
        "==" = guardOneBinaryOp "==" [builtinNames] l r;
        "!=" = guardOneBinaryOp "!=" [builtinNames] l r;
        "<" = guardOneBinaryOp "<" [["int" "float"] ["string"] ["path"] ["list"]] l r;
        ">" = guardOneBinaryOp ">" [["int" "float"] ["string"] ["path"] ["list"]] l r;
        "<=" = guardOneBinaryOp "<=" [["int" "float"] ["string"] ["path"] ["list"]] l r;
        ">=" = guardOneBinaryOp ">=" [["int" "float"] ["string"] ["path"] ["list"]] l r;
      });

  runBinaryOp = l: op: r: {_}:
    _.do
      (guardBinaryOp l op r)
      (switch op {
        "+" = apply2 (l: r: l + r) l r;
        "-" = apply2 (l: r: l - r) l r;
        "*" = apply2 (l: r: l * r) l r;
        "/" = apply2 (l: r: l / r) l r;
        "++" = apply2 (l: r: l ++ r) l r;
        "//" = apply2 (l: r: l // r) l r;
        "==" = apply2 (l: r: l == r) l r;
        "!=" = apply2 (l: r: l != r) l r;
        "<" = apply2 (l: r: l < r) l r;
        ">" = apply2 (l: r: l > r) l r;
        "<=" = apply2 (l: r: l <= r) l r;
        ">=" = apply2 (l: r: l >= r) l r;
      });

  runBinaryOpListwise = ls: op: rs: {_}:
    let lSnoc = maybeSnoc ls;
        rSnoc = maybeSnoc rs;
    in
      if lSnoc == null && rSnoc == null
      then (switch op {
        "<" = _.pure false;
        ">" = _.pure false;
        "<=" = _.pure false;
        ">=" = _.pure true;
      })
      else if lSnoc == null
      then (switch op {
        "<" = _.pure true;
        ">" = _.pure false;
        "<=" = _.pure true;
        ">=" = _.pure false;
      })
      else if rSnoc == null
      then (switch op {
        "<" = _.pure false;
        ">" = _.pure true;
        "<=" = _.pure false;
        ">=" = _.pure true;
      })
      else _.do
        {l = force lSnoc.head;}
        {r = force rSnoc.head;}
        ({_, l, r}: _.do
          {eq = runBinaryOp l "==" r;}
          ({eq, _}:
            if eq then _.bind (runBinaryOpListwise lSnoc.tail op rSnoc.tail)
            else _.bind (runBinaryOp l op r)));

  knownBinaryOps = [
    "+"
    "-"
    "*"
    "/"
    "++"
    "//"
    "=="
    "!="
    "<"
    ">"
    "<="
    ">="
    "&&"
    "||"
    "."
    "or"
  ];

  listwiseBinaryOps = [
    "<"
    ">"
    "<="
    ">="
  ];

  # Evaluate a binary operation
  # evalBinaryOp :: AST -> Eval a
  evalBinaryOp = node: {_, ...}:
    _.do
      (while "evaluating 'binaryOp' node")
      ({_}: _.guard (elem node.op knownBinaryOps) (RuntimeError ''
        Unsupported binary operator: ${node.op}
      ''))
      (
        if node.op == "." then evalAttributeAccess node
        else if node.op == "or" then evalOrOperation node
        else {_, ...}: _.do
          (while "evaluating binary operation LHS")
          {l = forceEvalNodeM node.lhs;}
          ({_, l, ...}:
            # && operator separated out to add short-circuiting.
            if node.op == "&&" then _.do
              (guard (lib.isBool l) (TypeError (_b_ ''
                &&: got non-bool left operand of type ${typeOf l}:
                  ${_ph_ l}
              '')))
              ({_}:
                if !l then _.pure false
                else _.do
                  (while "evaluating && RHS")
                  {r = forceEvalNodeM node.rhs;}
                  ({r, _}: _.do
                    (guard (lib.isBool r) (TypeError (_b_ ''
                      &&: got non-bool right operand of type ${typeOf r}:
                        ${_ph_ r}
                    '')))
                    (pure r)))

            # || operator separated out to add short-circuiting.
            else if node.op == "||" then _.do
              (guard (lib.isBool l) (TypeError (_b_ ''
                ||: got non-bool left operand of type ${typeOf l}:
                  ${_ph_ l}
              '')))
              ({_}:
                if l then _.pure true
                else _.do
                  (while "evaluating || RHS")
                  {r = forceEvalNodeM node.rhs;}
                  ({r, _}: _.do
                    (guard (lib.isBool r) (TypeError (_b_ ''
                      ||: got non-bool right operand of type ${typeOf r}:
                        ${_ph_ r}
                    '')))
                    (pure r)))

            # All other binary operators without short-circuiting.
            else _.do
              (while "evaluating binary operation RHS")
              {r = forceEvalNodeM node.rhs;}
              ({r, _}: _.do
                (guardBinaryOp l node.op r)
                (if isList l && isList r && elem node.op listwiseBinaryOps
                 then runBinaryOpListwise l node.op r
                 else runBinaryOp l node.op r))
        ));

  # obj.a.b.c - traverse the attribute path
  traversePath = obj: components: {_, ...}:
    _.do
      (while "traversing attr path")
      {keys = {_}: _.traverse identifierName components;}
      ({keys, _}: _.guard (keys == [] || lib.isAttrs obj) (TypeError ''
        attribute selection: expected attrset object on LHS of '.', got ${lib.typeOf obj}:
          ${_ph_ obj}.${joinSep "." keys}
      ''))
      ({keys, _}:
        let ks = maybeSnoc keys;
        in if ks == null then _.pure obj
        else _.do
          ({_}: _.guard (hasAttr ks.head obj) (MissingAttributeError ks.head))
          ({_, ...}: _.bind (traversePath obj.${ks.head} ks.tail)));

  # Evaluate attribute access (dot operator)
  # evalAttributeAccess :: AST -> Eval a
  evalAttributeAccess = node: {_, ...}:
    _.do
      (while "evaluating 'attribute access' node")
      {obj = evalNodeM node.lhs;}
      (
        # obj.a or b
        if node.rhs.nodeType == "binaryOp" && node.rhs.op == "or" then
          {_, obj, ...}: _.do
            { defaultVal = evalNodeM node.rhs.rhs; }
            { attrName = identifierName node.rhs.lhs; }
            ( {_, attrName, defaultVal, ...}: _.pure (obj.${attrName} or defaultVal) )

        # obj.a
        else if node.rhs.nodeType == "attrPath" then
          ({_, obj, ...} @ ctx: traversePath obj node.rhs.path ctx)

        else 
          _.throws (RuntimeError ''
            Unsupported attribute access: ${node.rhs.nodeType}
            (Expected 'attrPath' or 'binaryOp' with 'or' operator)
          ''));

  # evalOrOperation :: AST -> Eval a
  evalOrOperation = node: {_, ...}:
    (_.do
      (while "evaluating 'or' node")
      (guard (node.lhs.nodeType == "binaryOp" && node.lhs.op == ".") (RuntimeError ''
        Unsupported 'or' after non-select: ${node.lhs.nodeType} or ...
      ''))
      (evalNodeM node.lhs))
    .catch ({_, _e}: _.do
      (while "Handling missing attribute in 'or' node")
      (guard (MissingAttributeError.check _e) _e)
      (evalNodeM node.rhs));

  # evalUnaryOp :: AST -> Eval a
  evalUnaryOp = node: {_, ...}:
    _.do
      (while "evaluating 'unary' node")
      {operand = evalNodeM node.operand;}
      ({_, operand}: _.pure (switch node.op {
        "!" = (!operand);
        "-" = (-operand);
      }));

  # evalConditional :: AST -> Eval a
  evalConditional = node: {_, ...}:
    _.do
      (while "evaluating 'conditional' node")
      {cond = forceEvalNodeM node.cond;}
      ({_, cond}: _.do
        (guard (lib.isBool cond) (TypeError ''
          if: got non-bool condition of type ${lib.typeOf cond}:
            ${_ph_ cond}
        ''))
        (if cond
         then evalNodeM node."then"
         else evalNodeM node."else"));

  paramName = param: param.name.name;
  defaultParamAttrs = param: filter (paramAttr: paramAttr.nodeType == "defaultParam") param.attrs;
  requiredParamAttrs = param: filter (paramAttr: paramAttr.nodeType != "defaultParam") param.attrs;

  # TODO: Recursive defaults won't see each other here maybe
  getDefaultLambdaScope = param:
    {_, ...}: _.do
      {defaults =
        _.traverse
          (paramAttr:
            _.do
              {default = evalNodeM paramAttr.default;}
              ({_, default}: _.pure { ${paramName paramAttr} = default;}))
          (defaultParamAttrs param);}
      ({_, defaults, ...}: _.pure (mergeAttrsList defaults));

  # Return any extra scope bound by passing in the arg to the param.
  # evalLambdaParams :: AST -> a -> Eval Scope
  evalLambdaParams = param: arg: {_, ...}:
    _.do
      (while "evaluating 'lambda' parameters")
      ({_}: switch.on (p: p.nodeType) param {
        # Simple param is just a name, so just bind it to the arg.
        simpleParam = _.pure { ${paramName param} = arg; };

        # Attrset param is a set of names, so bind each to the arg, with defaults handled.
        attrSetParam = 
          let 
            allParamNames = map paramName param.attrs;
            requiredParamNames = map paramName (requiredParamAttrs param);
            suppliedUnknownNames = removeAttrs arg allParamNames;
            requiredUnsuppliedNames = filter (name: !(hasAttr name arg)) requiredParamNames;
          in
            _.do
              ({_}: _.guard (lib.isAttrs arg) (TypeError ''
                Expected attrset argument, got ${lib.typeOf arg}:
                  ${_ph_ arg}
              ''))
              ({_}: _.guard (empty requiredUnsuppliedNames) (RuntimeError ''
                Missing required parameters in attrset lambda: ${joinSep ", " requiredUnsuppliedNames}:
                  ${_ph_ param}
              ''))
              ({_}: _.guard (param.ellipsis || empty suppliedUnknownNames) (RuntimeError ''
                Unknown parameters in non-ellipsis attrset lambda: ${joinSep ", " (attrNames suppliedUnknownNames)}:
                  ${_ph_ param}
              ''))
              {defaults = getDefaultLambdaScope param; }
              ({_, defaults, ...}: _.pure (defaults // arg) );
      });

  # Carrier for lambdas created entirely inside the Eval monad, as opposed to e.g.
  # lambdas created by the use of the builtins embedded in the scope.
  # In these cases we can continue evaluating inside the Eval monad, lazily
  # binding the parameters to the arguments passed in and evaluating the body
  # inside the monad.
  #
  # For feature parity with Nix runtime typechecking/functor calling, if these are passed
  # in to native code (imports, builtins, lib, etc) we convert them to regular Nix lambdas first.
  #
  # We get deep conversion via the use of run_:
  # e.g. if we have Eval (a: b: a + b) then asLambda gives native a: (b: a + b).run ==
  # a: (b: a + b) (the latter EL is converted via asLambda in run_) so this maybe be okay.
  isEvaluatedLambda = x: x ? __isEvaluatedLambda;
  EvaluatedLambda = param: body: {_, ...}: 
    _.pure (lib.fix (self: {
      __isEvaluatedLambda = true;

      # Continue evaluating inside the Eval monad
      # Do not leak any scope updates inside the application to the outside.
      # saveScope here would cause any thunks forced inside the application
      # to remain thunks in the state, causing e.g. recursive functions to evaluate
      # the whole form every time we reach them. We instead need to saveScope around
      # function application itself.
      apply = arg:
        (_.do
          {paramScope = evalLambdaParams param arg;}
          ({paramScope, _, ...}: _.do
            (appendScope paramScope)
            (evalNodeM body)))
        .runClosureM;

      # Convert to a regular Nix lambda deeply, using self._ as snapshotted state.
      # Can't return lazy values as Nix thunks aren't exposed, so must force.
      asLambda = arg:
        (_.do
          {r = forceDeeplyM (self.apply arg);}
          ({r, _, ...}: _.pure (maybeConvertLambda r))
        ).runClosure {};
    }));

  # Evaluate a lambda expression
  # evalLambda :: AST -> Eval EvaluatedLambda
  evalLambda = node: {_, ...}:
    _.do
      (while "evaluating 'lambda' node")
      (EvaluatedLambda node.param node.body);

  isApplicable = x: builtins.isFunction x || x ? __functor || isEvaluatedLambda x;

  guardApplicable = func: {_, ...}:
    _.guard (isApplicable func) (TypeError ''
      Attempted to apply non-applicable value of type ${lib.typeOf func}:
        ${_ph_ func}
    '');

  # Apply an already-evaluated function to two already-evaluated arguments.
  # apply1 :: (EvaluatedLambda | function) -> a -> b -> Eval c
  apply2 = f: l: r: {_}: _.do
    {m = apply1 f l;}
    ({m, _}: _.bind (apply1 m r));

  # Apply an already-evaluated function to an already-evaluated argument.
  # apply1 :: (EvaluatedLambda | function) -> a -> Eval b
  apply1 = func: arg: {_, ...}:
    _.do
      (while "applying function to pre-evaluated argument")
      ( # If we are able to remain in the Eval monad, do not force the lambda.
        # Does not carry _ so that the original closure is preserverd
        if isEvaluatedLambda func then func.apply arg
        # If we are able to remain in the Eval monad, do not force the lambda.
        # First apply __functor self monadically, then again with the argument.
        else if func ? __functor then apply2 func.__functor func arg
        # Otherwise, just apply the function to the argument.
        else pure (func (maybeConvertLambda arg)));

  # Apply an already-evaluated function to an unevaluated argument.
  # apply1Node :: (EvaluatedLambda | function) -> AST -> Eval a
  apply1Node = func_: argNode: {_, ...}:
    _.do
      (while "applying function to unevaluated argument")
      # Force s.t. any recursive usage (i.e. descending into __functor) can supply
      # thunks.
      {func = force func_;}
      ({func, _}: _.do
        (guardApplicable func)
        {arg = forceEvalNodeM argNode;}
        ({arg, _}: _.bind (apply1 func arg)));

  # Evaluate function application.
  # evalApplication :: AST -> Eval a
  evalApplication = node: {_, ...}:
    _.do
      (while "evaluating 'application' node")
      {func = forceEvalNodeM node.func;}
      ({_, func, ...}: _.do
        (guardApplicable func)
        # saveScope to stop bound lambda arguments leaking
        (saveScope ({_}: _.foldM apply1Node func node.args)));

  # Evaluate a let expression
  # evalLetIn :: AST -> Eval a
  evalLetIn = node: {_}: 
    _.do
      (while "evaluating 'letIn' node")
      (saveScope ({_}: _.do
        (evalRecBindingList node.bindings)
        (evalNodeM node.body)));

  # Evaluate a with expression
  # evalWith :: AST -> Eval a
  evalWith = node: {_}:
    _.do
      (while "evaluating 'with' node")
      # Env as an attrset in WHNF
      {env = forceEvalNodeM node.env;}
      ({_, env}: _.do
        (guard (lib.isAttrs env) (TypeError ''
          with: got non-attrset environment of type ${typeOf env}:
            ${_ph_ env}
        ''))
        (saveScope ({_}: _.do
          (prependScope env)
          (evalNodeM node.body))));

  # Evaluate an assert expression
  # evalAssert :: AST -> Eval a
  evalAssert = node: {_, ...}:
    _.do
      (while "evaluating 'assert' node")
      {cond = evalNodeM node.cond;}
      ({_, cond}: _.guard (lib.isBool cond) (TypeError ''
        assert: got non-bool condition of type ${typeOf cond}:
          ${_ph_ cond}
      ''))
      ({_, cond}: _.guard cond (AssertError ''
        assert: condition failed:
          ${_ph_ cond}
      ''))
      (evalNodeM node.body);

  # Evaluate an abort expression
  # evalAbort :: Scope -> AST -> Eval a
  evalAbort = node: {_, ...}:
    _.do
      (while "evaluating 'abort' node")
      {msg = evalNodeM node.msg;}
      ({_, msg}: _.guard (lib.isString msg) (TypeError ''
        abort: got non-string message of type ${typeOf msg}:
          ${_ph_ msg}
      ''))
      ({_, msg}: _.throws (Abort msg));

  # Evaluate a throw expression
  # evalThrow :: Scope -> AST -> Eval a
  evalThrow = node: {_, ...}:
    _.do
      (while "evaluating 'throw' node")
      {msg = evalNodeM node.msg; }
      ({_, msg}: _.guard (lib.isString msg) (TypeError ''
        throw: got non-string message of type ${typeOf msg}:
          ${_ph_ msg}
      ''))
      (throws (Throw msg));

  # Evaluate an import expression
  # evalImport :: Scope -> AST -> Eval a
  evalImport = node: {_, ...}:
    _.do
      (while "evaluating 'import' node")
      {path = evalNodeM node.path;}
      ({_, path}: _.guard (lib.isString path || lib.isPath path) (TypeError ''
        import: got non-string or path message of type ${typeOf path}:
          ${_ph_ path}
      ''))
      (pure (import path));

  evalStringPieces = node: {_, ...}:
    _.do
      (while "evaluating 'stringPieces' node")
      {pieces = traverse evalNodeM node.pieces;}
      ({_, pieces}:
        let s = join pieces;
        in if node.indented
           then _.pure (setIndent 0 s)
           else _.pure s);

  evalPath = node: {_, ...}:
    _.do
      (while "evaluating 'path' node")
      {state = get;}
      ({state, _}: _.pure (stringToPath_ state.scope.PWD state.scope.HOME node.value));

  evalAnglePath = node: {_, ...}:
    _.do
      (while "evaluating 'anglePath' node")
      {scope = getScope;}
      ({_, scope}:
        let path = splitSep "/" node.value;
            name = maybeHead path;
            rest = maybeTail path;
            restPath = joinSep "/" (def [] rest);
        in _.do
          ({_}: _.guard (hasAttr "NIX_PATH" scope) (NixPathError ''
            No NIX_PATH found in scope when resolving ${node.value}.
          ''))
          ({_}: _.guard (hasAttr name scope.NIX_PATH) (NixPathError ''
            ${name} not found in NIX_PATH when resolving ${node.value}.
          ''))
          (pure (scope.NIX_PATH.${name} + "/${restPath}")));

  # Helper to test round-trip property: eval (parse x) == x
  testRoundTripLazy = testRoundTripWith evalAST collective-lib.tests.expect.noLambdasEq;
  testRoundTrip = testRoundTripWith evalAST' collective-lib.tests.expect.noLambdasEq;
  testRoundTripWith = evalASTFn: expectation: expr: expected: {
    # Just test that parsing succeeds and the result evaluates to expected
    roundTrip = 
      let result = evalASTFn expr;
      in expectation result ((Either EvalError (getT expected)).Right expected);
  };

  expectEvalError = expectEvalErrorWith collective-lib.tests.expect.noLambdasEq;
  expectEvalErrorWith = expectation: E: expr:
    let result = runAST' expr;
    in expectation (rec {
      resultIsLeft = isLeft result;
      resultEMatches = is E (result.left or null);
      inherit E;
      resultE = result.left or null;
    }) {
      resultIsLeft = true;
      resultEMatches = true;
      inherit E;
      resultE = result.left or null;
    };

  CODE = nodeType: {
    inherit nodeType;
    __isNodeThunk = true;
    __toString = collective-lib.tests.expect.anyLambda;
    force = collective-lib.tests.expect.anyLambda;
  };

  _tests = with tests; suite {

    # Tests for evalAST round-trip property
    evalAST = {

      _00_smoke.strict = solo {
        _00_int = testRoundTrip "1" 1;
        _01_float = testRoundTrip "1.0" 1.0;
        _02_string = testRoundTrip ''"hello"'' "hello";
        _03_indentString = testRoundTrip "''hello''" "hello";
        _04_true = testRoundTrip "true" true;
        _05_false = testRoundTrip "false" false;
        _06_null = testRoundTrip "null" null;
        _07_list = testRoundTrip "[1 2 3]" [1 2 3];
        _08_attrSet = testRoundTrip "{a = 1;}" {a = 1;};
        _09_attrPath = testRoundTrip "{a = 1;}.a" 1;
        _10_attrPathOr = testRoundTrip "{a = 1;}.b or 2" 2;
        _11_inheritsConst = testRoundTrip "{ inherit ({a = 1;}) a; }" {a = 1;};
        _12_recAttrSetNoRecursion = testRoundTrip "rec { a = 1; }" {a = 1;};
        _13_recAttrSetRecursion.access = testRoundTrip "(rec { a = 1; b = a; }).b" 1;
        _13_recAttrSetRecursionBackwards.define = testRoundTrip "rec { a = b; b = 1; }" {a = 1; b = 1;};
        _13_recAttrSetRecursionBackwards.access = testRoundTrip "(rec { a = b; b = 1; }).a" 1;
        _14_recAttrSetNested = testRoundTrip "rec { a = 1; b = { c = a; }; }" { a = 1; b = { c = 1; };};
        _15_recAttrSetNestedRec = testRoundTrip "rec { a = 1; b = rec { c = a; }; }" { a = 1; b = { c = 1; };};
        _16_letIn = testRoundTrip "let a = 1; in a" 1;
        _17_letInNested = testRoundTrip "let a = 1; in let b = a + 1; in [a b]" [1 2];
        _18_withs = testRoundTrip "with {a = 1;}; a" 1;
        _19_withsNested = testRoundTrip "with {a = 1;}; with {b = a + 1;}; [a b]" [1 2];
        _20_lambda = testRoundTrip "(a: b: a + b) 1 2" 3;
        _21_lambdaClosure = testRoundTrip "let a = 1; f = b: a + b; in let a = 100; in f 2" 3;
      };

      _00_smoke.lazy = solo {
        _00_int = testRoundTripLazy "1" 1;
         _01_float = testRoundTripLazy "1.0" 1.0;
         _02_string = testRoundTripLazy ''"hello"'' "hello";
         _03_indentString = testRoundTripLazy "''hello''" "hello";
         _04_true = testRoundTripLazy "true" true;
         _05_false = testRoundTripLazy "false" false;
         _06_null = testRoundTripLazy "null" null;
         _07_list = testRoundTripLazy "[1 2 3]" [(CODE "int") (CODE "int") (CODE "int")];
         _08_attrSet = testRoundTripLazy "{a = 1;}" {a = CODE "int";};
         _09_attrPath = testRoundTripLazy "{a = 1;}.a" 1;
         _10_attrPathOr = testRoundTripLazy "{a = 1;}.b or 2" 2;
         _11_inheritsConst = testRoundTripLazy "{ inherit ({a = 1;}) a; }" {a = CODE "int";};
         _12_recAttrSetNoRecursion = testRoundTripLazy "rec { a = 1; }" {a = CODE "int";};
         _13_recAttrSetRecursion.define =
          testRoundTripLazy "rec { a = 1; b = a; }" {a = CODE "int"; b = CODE "identifier";};
         _13_recAttrSetRecursion.access =
          testRoundTripLazy "(rec { a = 1; b = a; }).b" 1;
         _13_recAttrSetRecursionBackwards.define =
           testRoundTripLazy "rec { a = b; b = 1; }" {a = CODE "identifier"; b = CODE "int";};
         _13_recAttrSetRecursionBackwards.access =
           testRoundTripLazy "(rec { a = b; b = 1; }).a" 1;
         _14_recAttrSetNested =
           testRoundTripLazy "rec { a = 1; b = { c = a; }; }" { a = CODE "int"; b = CODE "attrs"; };
         _15_recAttrSetNestedRec =
          testRoundTripLazy "rec { a = 1; b = rec { c = a; }; }" { a = CODE "int"; b = CODE "attrs"; };
         _16_letIn = testRoundTripLazy "let a = 1; in a" 1;
         _17_letInNested =
          testRoundTripLazy "let a = 1; in let b = a + 1; in [a b]" [(CODE "identifier") (CODE "identifier")];
         _18_withs =
          testRoundTripLazy "with {a = 1;}; a" 1;
         _19_withsNested =
          testRoundTripLazy "with {a = 1;}; with {b = a + 1;}; [a b]" [(CODE "identifier") (CODE "identifier")];
         _20_lambda = testRoundTripLazy "(a: b: a + b) 1 2" 3;
         _21_lambdaClosure = testRoundTripLazy "let a = 1; f = b: a + b; in let a = 100; in f 2" 3;
      };

      _01_allFeatures =
        (
        let 
          # Test all major language constructs in one expression
          expr = ''
            let f = { a ? 1, b, ... }:
                  let 
                    data = { 
                      aa = a;
                      inherit b;
                    }; 
                  in with data; aa + b; 
            in f {b = 4;}
          '';
        in testRoundTrip expr 5
        );

      # Basic literals
      _02_literals = {
        integers = {
          positive = testRoundTrip "42" 42;
          negative = testRoundTrip "-42" (-42);
          zero = testRoundTrip "0" 0;
          large = testRoundTrip "999999999" 999999999;
          veryLarge = testRoundTrip "9223372036854775807" 9223372036854775807;
        };
        floats = {
          positive = testRoundTrip "3.14" 3.14;
          negative = testRoundTrip "-3.14" (-3.14);
          zero = testRoundTrip "0.0" 0.0;
          scientific = testRoundTrip "1.23e10" 1.23e10;
          scientificNegative = testRoundTrip "1.23e-10" 1.23e-10;
          fractional = testRoundTrip ".5" 0.5;
          trailingZero = testRoundTrip "1.0" 1.0;
        };
        strings = {
          simple = testRoundTrip ''"hello"'' "hello";
          empty = testRoundTrip ''""'' "";
          withSpaces = testRoundTrip ''"hello world"'' "hello world";
          withEscapes = testRoundTrip ''"hello\nworld"'' "hello\nworld";
          withQuotes = testRoundTrip ''"say \"hello\""'' "say \"hello\"";
          withBackslash = testRoundTrip ''"path\\to\\file"'' "path\\to\\file";
          withTab = testRoundTrip ''"hello\tworld"'' "hello\tworld";
          unicode = testRoundTrip ''"λ hello 世界"'' "λ hello 世界";
          multiline = testRoundTrip "''hello\nworld''" "hello\nworld";
          indentedString = testRoundTrip "''  hello\n  world''" "hello\nworld";
          emptyIndented = testRoundTrip "''''" "";
          singleChar = testRoundTrip ''"a"'' "a";
          numbers = testRoundTrip ''"123"'' "123";
          specialChars = testRoundTrip ''"!@#$%^&*()"'' "!@#$%^&*()";
        };
        booleans = {
          true = testRoundTrip "true" true;
          false = testRoundTrip "false" false;
        };
        nullValue = testRoundTrip "null" null;
        paths = {
          relative = testRoundTrip "./test" /tmp/pwd/test;
          absolute = testRoundTrip "/tmp/test" /tmp/test;
          home = testRoundTrip "~/test" /tmp/home/test;
          nested = testRoundTrip "./a/b/c" /tmp/pwd/a/b/c;
        };
      };

      # Collections
      _03_lists = {
        empty = testRoundTrip "[]" [];
        single = testRoundTrip "[1]" [1];
        numbers = testRoundTrip "[1 2 3]" [1 2 3];
        mixed = testRoundTrip ''[1 "hello" true]'' [1 "hello" true];
        nested = testRoundTrip "[[1 2] [3 4]]" [[1 2] [3 4]];
        deeplyNested = testRoundTrip "[[[1]]]" [[[1]]];
        withNulls = testRoundTrip "[1 null 3]" [1 null 3];
        withBooleans = testRoundTrip "[true false true]" [true false true];
        withStrings = testRoundTrip ''["a" "b" "c"]'' ["a" "b" "c"];
        withFloats = testRoundTrip "[1.1 2.2 3.3]" [1.1 2.2 3.3];
        largeNumbers = testRoundTrip "[999999999 (-999999999)]" [999999999 (-999999999)];
        emptyStrings = testRoundTrip ''["" "hello" ""]'' ["" "hello" ""];
        whitespace = testRoundTrip "[ 1 2 3 ]" [1 2 3];
        nestedEmpty = testRoundTrip "[[] [1] []]" [[] [1] []];
        mixedNested = testRoundTrip ''[1 [2 "three"] [true [null]]]'' [1 [2 "three"] [true [null]]];
        expressions = testRoundTrip "[(1 + 1) (2 * 3)]" [2 6];
        withAttributes = testRoundTrip "[{a = 1;} {b = 2;}]" [{a = 1;} {b = 2;}];
        singletonNested = testRoundTrip "[[1]]" [[1]];
        alternatingTypes = testRoundTrip ''[1 "a" 2 "b"]'' [1 "a" 2 "b"];
        largeList = testRoundTrip "[1 2 3 4 5 6 7 8 9 10]" [1 2 3 4 5 6 7 8 9 10];
      };

      _04_attrs = {
        empty = testRoundTrip "{}" {};
        simple = testRoundTrip "{ a = 1; b = 2; }" { a = 1; b = 2; };
        nested = testRoundTrip "{ x = { y = 42; }; }" { x = { y = 42; }; };
        recursive = {
          complex = testRoundTrip "rec { a = 1; b = a + 1; c = b + a; }" { a = 1; b = 2; c = 3; };
          mutualRecursion = testRoundTrip "rec { a = b + 1; b = 5; }" { a = 6; b = 5; };
          nested = testRoundTrip "rec { x = { y = z; }; z = 42; }" { x = { y = 42; }; z = 42; };
          #selfReference = testRoundTrip "rec { a = { b = 1; c = a.b; }; }" {a = { b = 1; c = 1; };};
          #selfReferenceFn = testRoundTrip "(rec { f = x: if x == 0 then 1 else x * f (x - 1); }).f 3" 6;
          simple = testRoundTrip "rec { a = 1; b = a; }" { a = 1; b = 1; };
        };
        inheritance = {
          simple = testRoundTrip "let x = 1; in { inherit x; }" { x = 1; };
          multiple = testRoundTrip "let x = 1; y = 2; in { inherit x y; }" { x = 1; y = 2; };
          fromAttrSet = testRoundTrip "{ inherit ({a = 1; b = 2;}) a b; }" { a = 1; b = 2; };
          mixed = testRoundTrip "let x = 3; in { inherit x; y = 4; }" { x = 3; y = 4; };
          nested = testRoundTrip "let attrs = {a = 1;}; in { inherit (attrs) a; }" { a = 1; };
        };
        expressions = {
          arithmetic = testRoundTrip "{ a = 1 + 2; b = 3 * 4; }" { a = 3; b = 12; };
          conditionals = testRoundTrip "{ a = if true then 1 else 2; }" { a = 1; };
          lists = testRoundTrip "{ a = [1 2]; b = []; }" { a = [1 2]; b = []; };
        };
        specialNames = {
          keywords = testRoundTrip ''{ "if" = 1; "then" = 2; "else" = 3; }'' { "if" = 1; "then" = 2; "else" = 3; };
          numbers = testRoundTrip ''{ "123" = "numeric"; }'' { "123" = "numeric"; };
          spaces = testRoundTrip ''{ "hello world" = 1; }'' { "hello world" = 1; };
          symbols = testRoundTrip ''{ "@#$" = "symbols"; }'' { "@#$" = "symbols"; };
        };
        whitespace = {
          spaces = testRoundTrip "{ a = 1 ; b = 2 ; }" { a = 1; b = 2; };
          newlines = testRoundTrip "{\n  a = 1;\n  b = 2;\n}" { a = 1; b = 2; };
          tabs = testRoundTrip "{\ta = 1;\tb = 2;}" { a = 1; b = 2; };
        };
        deepNesting = {
          threeLevels = testRoundTrip "{ a = { b = { c = 42; }; }; }" { a = { b = { c = 42; }; }; };
          fourLevels = testRoundTrip "{ a = { b = { c = { d = 1; }; }; }; }" { a = { b = { c = { d = 1; }; }; }; };
          mixed = testRoundTrip "{ a = { b = [1 2]; c = { d = true; }; }; }" { a = { b = [1 2]; c = { d = true; }; }; };
        };
        shadowing = {
          letShadowsGlobal = testRoundTrip "let a = 1; in { a = 2; result = a; }" { a = 2; result = 1; };
          innerShadowsOuter = testRoundTrip "{ a = 1; inner = { a = 2; }; }" { a = 1; inner = { a = 2; }; };
        };
        withNull = {
          nullValue = testRoundTrip "{ a = null; }" { a = null; };
          mixedWithNull = testRoundTrip "{ a = 1; b = null; c = true; }" { a = 1; b = null; c = true; };
        };
        allTypes = testRoundTrip ''{ 
          int = 42; 
          float = 3.14; 
          string = "hello"; 
          bool = true; 
          null = null; 
          list = [1 2]; 
          attrs = { nested = true; }; 
        }'' { 
          int = 42; 
          float = 3.14; 
          string = "hello"; 
          bool = true; 
          null = null; 
          list = [1 2]; 
          attrs = { nested = true; }; 
        };
      };

      # Binary operations
      _05_arithmetic = {
        addition = {
          simple = testRoundTrip "1 + 2" 3;
          floats = testRoundTrip "1.5 + 2.5" 4.0;
          negative = testRoundTrip "(-1) + (-2)" (-3);
          zero = testRoundTrip "0 + 5" 5;
          floatInt = testRoundTrip "1.5 + 2" 3.5;
          intFloat = testRoundTrip "2 + 1.5" 3.5;
          large = testRoundTrip "999999999 + 1" 1000000000;
          stringConcat = testRoundTrip ''"hello" + " world"'' "hello world";
          pathConcat = testRoundTrip "./hello + /world" /tmp/pwd/hello/world;
          mixed = testRoundTrip "1 + 2 + 3" 6;
          precedence = testRoundTrip "1 + 2 * 3" 7;
        };
        subtraction = {
          simple = testRoundTrip "10 - 3" 7;
          floats = testRoundTrip "5.5 - 2.5" 3.0;
          negative = testRoundTrip "(-5) - (-3)" (-2);
          zero = testRoundTrip "5 - 0" 5;
          floatInt = testRoundTrip "5.5 - 2" 3.5;
          intFloat = testRoundTrip "5 - 2.5" 2.5;
          toNegative = testRoundTrip "3 - 5" (-2);
          chain = testRoundTrip "10 - 3 - 2" 5;
        };
        multiplication = {
          simple = testRoundTrip "3 * 4" 12;
          floats = testRoundTrip "2.5 * 3.0" 7.5;
          negative = testRoundTrip "(-3) * 4" (-12);
          zero = testRoundTrip "5 * 0" 0;
          one = testRoundTrip "7 * 1" 7;
          floatInt = testRoundTrip "2.5 * 4" 10.0;
          intFloat = testRoundTrip "4 * 2.5" 10.0;
          large = testRoundTrip "999 * 1000" 999000;
          chain = testRoundTrip "2 * 3 * 4" 24;
          precedence = testRoundTrip "2 + 3 * 4" 14;
        };
        division = {
          simple = testRoundTrip "8 / 2" 4;
          floats = testRoundTrip "7.5 / 2.5" 3.0;
          intToFloat = testRoundTrip "7 / 2" 3;
          negative = testRoundTrip "(-8) / 2" (-4);
          negativeNumerator = testRoundTrip "8 / (-2)" (-4);
          floatInt = testRoundTrip "7.5 / 3" 2.5;
          intFloat = testRoundTrip "8 / 2.0" 4.0;
          fractional = testRoundTrip "1 / 3" 0;
          chain = testRoundTrip "24 / 4 / 2" 3;
          precedence = testRoundTrip "8 / 2 + 1" 5;
        };
        listOps = {
          concat = testRoundTrip "[1 2] ++ [3 4]" [1 2 3 4];
          empty = testRoundTrip "[] ++ [1 2]" [1 2];
          emptyRight = testRoundTrip "[1 2] ++ []" [1 2];
          nested = testRoundTrip "[[1]] ++ [[2]]" [[1] [2]];
          mixed = testRoundTrip ''[1 "a"] ++ [true null]'' [1 "a" true null];
          chain = testRoundTrip "[1] ++ [2] ++ [3]" [1 2 3];
        };
        attrOps = {
          merge = testRoundTrip "{a = 1;} // {b = 2;}" {a = 1; b = 2;};
          override = testRoundTrip "{a = 1; b = 2;} // {b = 3;}" {a = 1; b = 3;};
          empty = testRoundTrip "{} // {a = 1;}" {a = 1;};
          emptyRight = testRoundTrip "{a = 1;} // {}" {a = 1;};
          nested = testRoundTrip "{a = {x = 1;};} // {b = {y = 2;};}" {a = {x = 1;}; b = {y = 2;};};
          chain = testRoundTrip "{a = 1;} // {b = 2;} // {c = 3;}" {a = 1; b = 2; c = 3;};
          complex = testRoundTrip "{a = 1; b = 2;} // {b = 3; c = 4;}" {a = 1; b = 3; c = 4;};
        };
        associativity = {
          leftAssoc = testRoundTrip "10 - 5 - 2" 3;
          rightAssoc = testRoundTrip "2 + 3 + 4" 9;
          mixed = testRoundTrip "2 * 3 + 4" 10;
          parentheses = testRoundTrip "2 * (3 + 4)" 14;
        };
        edgeCases = {
          largeNumbers = testRoundTrip "9007199254740991 + 1" 9007199254740992;
          verySmallFloat = testRoundTrip "0.000001 * 1000000" 1.0;
          zeroOperations = testRoundTrip "0 + 0 - 0 * 5" 0;
          negativeZero = testRoundTrip "-0" 0;
        };
      };

      _06_logical = {
        and = {
          trueFalse = testRoundTrip "true && false" false;
          trueTrue = testRoundTrip "true && true" true;
          falseFalse = testRoundTrip "false && false" false;
          falseTrue = testRoundTrip "false && true" false;
          shortCircuit = testRoundTrip "false && (abort \"should not evaluate\")" false;
          chain = testRoundTrip "true && true && false" false;
          allTrue = testRoundTrip "true && true && true" true;
          precedence = testRoundTrip "true || false && false" true;
          parentheses = testRoundTrip "(true || false) && false" false;
          nonBoolFirst = expectEvalError TypeError "1 && true";
          nonBoolSecond = expectEvalError TypeError "true && 1";
          bothNonBool = expectEvalError TypeError "1 && 2";
        };
        or = {
          trueFalse = testRoundTrip "true || false" true;
          trueTrue = testRoundTrip "true || true" true;
          falseFalse = testRoundTrip "false || false" false;
          falseTrue = testRoundTrip "false || true" true;
          shortCircuit = testRoundTrip "true || (abort \"should not evaluate\")" true;
          chain = testRoundTrip "false || false || true" true;
          allFalse = testRoundTrip "false || false || false" false;
          precedence = testRoundTrip "false && true || true" true;
          parentheses = testRoundTrip "false && (true || true)" false;
          nonBoolFirst = expectEvalError TypeError "1 || true";
          nonBoolSecond = expectEvalError TypeError "false || 1";
          bothNonBool = expectEvalError TypeError "1 || 2";
        };
        mixed = {
          andOr = testRoundTrip "true && false || true" true;
          orAnd = testRoundTrip "false || true && false" false;
          complex = testRoundTrip "(true || false) && (false || true)" true;
          nested = testRoundTrip "true && (false || true)" true;
          doubleNegative = testRoundTrip "!(!true)" true;
          withComparison = testRoundTrip "(1 == 1) && (2 > 1)" true;
          withArithmetic = testRoundTrip "(1 + 1 == 2) || false" true;
        };
        edgeCases = {
          multipleParens = testRoundTrip "((true))" true;
          deepNesting = testRoundTrip "true && (false || (true && true))" true;
          allOperators = testRoundTrip "true && true || false && false" true;
          precedenceTest = testRoundTrip "true || false && false || true" true;
        };
      };

      _07_comparison = {
        equality = {
          attrEqual = testRoundTrip "{a = 1;} == {a = 1;}" true;
          attrNotEqual = testRoundTrip "{a = 1;} == {a = 2;}" false;
          boolEqual = testRoundTrip "true == true" true;
          boolNotEqual = testRoundTrip "true == false" false;
          emptyAttrEqual = testRoundTrip "{} == {}" true;
          emptyEqual = testRoundTrip "[] == []" true;
          floatEqual = testRoundTrip "1.0 == 1.0" true;
          floatIntEqual = testRoundTrip "1.0 == 1" true;
          floatNotEqual = testRoundTrip "1.0 == 2.0" false;
          intEqual = testRoundTrip "1 == 1" true;
          intFloatEqual = testRoundTrip "1 == 1.0" true;
          intNotEqual = testRoundTrip "1 == 2" false;
          #lambda = testRoundTrip "(a: a) == (b: b)" false;
          lambdaSelf = testRoundTrip "let f = (a: a); in f == f" false;
          listEqual = testRoundTrip "[1 2] == [1 2]" true;
          listNotEqual = testRoundTrip "[1 2] == [2 1]" false;
          nullEqual = testRoundTrip "null == null" true;
          nullNotEqual = testRoundTrip "null == 1" false;
          parentheses = testRoundTrip "(1 == 1)" true;
          stringEqual = testRoundTrip ''"hello" == "hello"'' true;
          stringNotEqual = testRoundTrip ''"hello" == "world"'' false;
        };
        inequality = {
          intNotEqual = testRoundTrip "1 != 2" true;
          intEqual = testRoundTrip "1 != 1" false;
          floatNotEqual = testRoundTrip "1.0 != 2.0" true;
          floatEqual = testRoundTrip "1.0 != 1.0" false;
          stringNotEqual = testRoundTrip ''"hello" != "world"'' true;
          stringEqual = testRoundTrip ''"hello" != "hello"'' false;
          boolNotEqual = testRoundTrip "true != false" true;
          boolEqual = testRoundTrip "true != true" false;
          nullNotEqual = testRoundTrip "null != 1" true;
          nullEqual = testRoundTrip "null != null" false;
        };
        ordering = {
          intLess = testRoundTrip "1 < 2" true;
          intNotLess = testRoundTrip "2 < 1" false;
          intLessEqual = testRoundTrip "1 <= 1" true;
          intLessEqualFalse = testRoundTrip "2 <= 1" false;
          intGreater = testRoundTrip "3 > 2" true;
          intNotGreater = testRoundTrip "2 > 3" false;
          intGreaterEqual = testRoundTrip "2 >= 2" true;
          intGreaterEqualFalse = testRoundTrip "1 >= 2" false;
          floatLess = testRoundTrip "1.5 < 2.5" true;
          floatGreater = testRoundTrip "2.5 > 1.5" true;
          intFloatLess = testRoundTrip "1 < 1.5" true;
          floatIntGreater = testRoundTrip "1.5 > 1" true;
          stringLess = testRoundTrip ''"a" < "b"'' true;
          stringGreater = testRoundTrip ''"b" > "a"'' true;
          stringEqual = testRoundTrip ''"a" <= "a"'' true;
        };
        edgeCases = {
          zero = testRoundTrip "0 == 0" true;
          negativeZero = testRoundTrip "(-0) == 0" true;
          negativeComparison = testRoundTrip "(-1) < 0" true;
          largeNumbers = testRoundTrip "999999999 == 999999999" true;
          veryLargeNumbers = testRoundTrip "9223372036854775807 == 9223372036854775807" true;
          floatPrecision = testRoundTrip "0.1 + 0.2 == 0.3" false;
          emptyString = testRoundTrip ''"" == ""'' true;
          emptyVsNull = testRoundTrip ''"" == null'' false;
          boolVsInt = testRoundTrip "true == 1" false;
          boolVsString = testRoundTrip ''true == "true"'' false;
          #pointerEqualityTrickDoesNotWork = testRoundTrip ''
          #  let pointerEqual = a: b: [ a ] == [ b ];
          #      f = a: a;
          #      g = a: a;
          #  in [(pointerEqual f f) (pointerEqual f g)]
          #'' [false false];
        };
        nested = {
          listOrdering.one = testRoundTrip "[1] < [2]" true;
          listOrdering.two = testRoundTrip "[1 2] < [2 1]" true;
          listOrdering.twoRev = testRoundTrip "[1 1] < [1 2]" true;
          listOrdering.twoRevGT = testRoundTrip "[1 1] > [1 2]" false;
          attrOrdering = expectEvalError TypeError "{a = 1;} < {a = 2;}";
          nestedAttrs = testRoundTrip "{a = {b = 1;};} == {a = {b = 1;};}" true;
          nestedLists = testRoundTrip "[[1 2]] == [[1 2]]" true;
          mixedNested = testRoundTrip ''[{a = 1;}] == [{a = 1;}]'' true;
        };
        chains = {
          comparison = testRoundTrip "1 < 2 && 2 < 3" true;
          mixed = testRoundTrip "1 == 1 && 2 != 3" true;
          precedence = testRoundTrip "1 + 1 == 2" true;
          complex = testRoundTrip "(1 + 2) == 3 && (4 - 1) == 3" true;
        };
      };

      # Unary operations
      _08_unary = {
        not = {
          notFalse = testRoundTrip "!false" true;
          notTrue = testRoundTrip "!true" false;
          doubleNot = testRoundTrip "!!true" true;
          tripleNot = testRoundTrip "!!!false" true;
          withParens = testRoundTrip "!(true)" false;
          withComparison = testRoundTrip "!(1 == 2)" true;
          withLogical = testRoundTrip "!(true && false)" true;
          complex = testRoundTrip "!(1 < 2 && 3 > 4)" true;
          precedence = testRoundTrip "!true || false" false;
          precedenceWithParens = testRoundTrip "!(true || false)" false;
        };
        negation = {
          negativeInt = testRoundTrip "-42" (-42);
          negativeFloat = testRoundTrip "-3.14" (-3.14);
          negativeZero = testRoundTrip "-0" 0;
          doubleNegative = testRoundTrip "-(-5)" 5;
          tripleNegative = testRoundTrip "-(-(-3))" (-3);
          withParens = testRoundTrip "-(42)" (-42);
          withArithmetic = testRoundTrip "-(1 + 2)" (-3);
          withComparison = testRoundTrip "-(1)" (-1);
          largeNumber = testRoundTrip "-999999999" (-999999999);
          floatPrecision = testRoundTrip "-0.000001" (-0.000001);
          scientificNotation = testRoundTrip "-1.23e10" (-1.23e10);
        };
        combined = {
          notAndNegation = testRoundTrip "!(-1 == -1)" false;
          negationAndComparison = testRoundTrip "-(1) == (-1)" true;
          complexNesting = testRoundTrip "!(-(1) == 1)" true;
          precedence = testRoundTrip "-1 + 2" 1;
          precedenceWithParens = testRoundTrip "-(1 + 2)" (-3);
          multipleUnary = testRoundTrip "!(!true) && -(-1) > 0" true;
        };
        edgeCases = {
          notWithArithmetic = testRoundTrip "!(1 + 1 == 3)" true;
          negativeInList = testRoundTrip "[(-1) (-2) (-3)]" [(-1) (-2) (-3)];
          negativeInAttrs = testRoundTrip "{a = (-1); b = (-2);}" {a = (-1); b = (-2);};
          notInConditional = testRoundTrip "if !false then 1 else 2" 1;
          negativeInConditional = testRoundTrip "if (-1) < 0 then 1 else 2" 1;
        };
      };

      # Conditionals
      _09_conditionals = {
        simple = {
          trueCondition = testRoundTrip "if true then 1 else 2" 1;
          falseCondition = testRoundTrip "if false then 1 else 2" 2;
          withParens = testRoundTrip "if (true) then 1 else 2" 1;
          withComparison = testRoundTrip "if 1 == 1 then 42 else 0" 42;
          withLogical = testRoundTrip "if true && false then 1 else 2" 2;
          withArithmetic = testRoundTrip "if 1 + 1 == 2 then 100 else 0" 100;
        };
        nested = {
          elseIf = testRoundTrip "if false then 1 else if true then 2 else 3" 2;
          deepNesting = testRoundTrip "if false then 1 else if false then 2 else if true then 3 else 4" 3;
          allFalse = testRoundTrip "if false then 1 else if false then 2 else 3" 3;
          nestedInThen = testRoundTrip "if true then (if true then 1 else 2) else 3" 1;
          nestedInElse = testRoundTrip "if false then 1 else (if true then 2 else 3)" 2;
          bothNested = testRoundTrip "if true then (if false then 1 else 2) else (if true then 3 else 4)" 2;
        };
        expressions = {
          stringResult = testRoundTrip ''if true then "yes" else "no"'' "yes";
          listResult = testRoundTrip "if true then [1 2] else [3 4]" [1 2];
          attrResult = testRoundTrip "if true then {a = 1;} else {b = 2;}" {a = 1;};
          nullResult = testRoundTrip "if false then 1 else null" null;
          #functionResult = testRoundTrip "if true then (x: x + 1) else (x: x - 1)" (x: x + 1);
        };
        complexConditions = {
          multipleComparisons = testRoundTrip "if 1 < 2 && 3 > 2 then 1 else 0" 1;
          negation = testRoundTrip "if !(1 == 2) then 1 else 0" 1;
          orCondition = testRoundTrip "if false || true then 1 else 0" 1;
          precedence = testRoundTrip "if true && false || true then 1 else 0" 1;
          withParens = testRoundTrip "if (true && false) || true then 1 else 0" 1;
        };
        typeVariations = {
          intConditions = expectEvalError TypeError "if 1 then 42 else 0";
          zeroCondition = expectEvalError TypeError "if 0 then 1 else 2";
          stringCondition = expectEvalError TypeError ''if "hello" then 1 else 2'';
          emptyStringCondition = expectEvalError TypeError ''if "" then 1 else 2'';
          listCondition = expectEvalError TypeError "if [1] then 1 else 2";
          emptyListCondition = expectEvalError TypeError "if [] then 1 else 2";
          attrCondition = expectEvalError TypeError "if {a = 1;} then 1 else 2";
          emptyAttrCondition = expectEvalError TypeError "if {} then 1 else 2";
          nullCondition = expectEvalError TypeError "if null then 1 else 2";
        };
        contextual = {
          inLet = testRoundTrip "let x = true; in if x then 1 else 2" 1;
          inAttr = testRoundTrip "{result = if true then 1 else 2;}" {result = 1;};
          inList = testRoundTrip "[(if true then 1 else 2)]" [1];
          inFunction = testRoundTrip "(x: if x then 1 else 2) true" 1;
          inArithmetic = testRoundTrip "(if true then 1 else 2) + 3" 4;
          asComparison = testRoundTrip "(if true then 1 else 2) == 1" true;
        };
        edgeCases = {
          sameTypeResults = testRoundTrip "if true then 1 else 1" 1;
          differentTypes = testRoundTrip ''if true then 1 else "hello"'' 1;
          complexExpressions = testRoundTrip "if (let x = 1; in x == 1) then (1 + 2) else (3 * 4)" 3;
          # Skip - self-recursion
          # recursiveCondition = testRoundTrip "let f = x: if x == 0 then 1 else x * f (x - 1); in f 3" 6;
          withScope = testRoundTrip "let a = 1; in if true then a + 1 else a - 1" 2;
        };
      };

      # Let expressions
      _10_letExpressions = {
        simple = {
          basic = testRoundTrip "let x = 1; in x" 1;
          multipleBindings = testRoundTrip "let a = 1; b = 2; in a + b" 3;
          withString = testRoundTrip ''let msg = "hello"; in msg'' "hello";
          withList = testRoundTrip "let xs = [1 2 3]; in xs" [1 2 3];
          withAttrs = testRoundTrip "let obj = {a = 1;}; in obj" {a = 1;};
          withFunction = testRoundTrip "let f = x: x + 1; in f 5" 6;
          withFunctions = testRoundTrip "let a = 1; f = x: x + a; in f 5" 6;
        };
        nested = {
          basic = testRoundTrip "let x = 1; y = let z = 2; in z + 1; in x + y" 4;
          deep = testRoundTrip "let a = let b = let c = 1; in c + 1; in b + 1; in a + 1" 4;
          independent = testRoundTrip "let x = (let y = 1; in y); z = (let w = 2; in w); in x + z" 3;
          dependent = testRoundTrip "let x = 1; y = let z = x + 1; in z * 2; in y" 4;
        };
        recursive = {
          simple = testRoundTrip "let x = y; y = 1; in x" 1;
          mutual = testRoundTrip "let a = b + 1; b = 5; in a" 6;
          complex = testRoundTrip "let a = b + c; b = 2; c = 3; in a" 5;
          factorial =
            let expr = i: "let f = x: if x == 0 then 1 else x * f (x - 1); in f ${toString i}";
            in {
              _0 = testRoundTrip (expr 0) 1;
              _1 = testRoundTrip (expr 1) 1;
              _2 = testRoundTrip (expr 2) 2;
              _3 = testRoundTrip (expr 3) 6;
              _4 = testRoundTrip (expr 4) 24;
            };
          fibonacci =
            let expr = i: "let fib = n: if n <= 1 then n else fib (n - 1) + fib (n - 2); in fib ${toString i}";
            in {
              _0 = testRoundTrip (expr 0) 0;
              _1 = testRoundTrip (expr 1) 1;
              _2 = testRoundTrip (expr 2) 1;
              _3 = testRoundTrip (expr 3) 2;
            };
          # TODO: Fixing this requires laziness; when fib2 calls into memo, memo is fully eval'd,
          # including fib2. We should instead have 'attrs' evaluate to an object that can be
          # accessed lazily i.e. most evaluations should return thunks.
          fibonacciRec =
            let expr = ''
              let k = i: "_" + (builtins.toString i);
                  fibm = i: builtins.getAttr (k i) memo;
                  fib = i: fibm (i - 1) + fibm (i - 2);
                  memo = {
                    _0 = 1;
                    _1 = 1;
                    _2 = fib 2;
                    #_3 = fib 3;
                    #_4 = fib 4;
                    #_5 = fib 5;
                    #_6 = fib 6;
                    #_7 = fib 7;
                  };
              in lib.attrValues memo
            '';
            in (testRoundTrip expr [0 1 1 2 3 5 8 13]);
        };
        shadowing = {
          innerShadowsOuter = testRoundTrip "let x = 1; in let x = 2; in x" 2;
          accessOuter = testRoundTrip "let x = 1; y = x + 1; in let x = 3; in y" 2;
          multipleLevels = testRoundTrip "let x = 1; in let x = 2; in let x = 3; in x" 3;
          partialShadowing = testRoundTrip "let x = 1; y = 2; in let x = 3; z = x + y; in z" 5;
        };
        expressions = {
          arithmetic = testRoundTrip "let x = 1 + 2; y = x * 3; in y" 9;
          conditionals = testRoundTrip "let result = if true then 1 else 2; in result" 1;
          withScope = testRoundTrip "let x = 1; result = if x == 1 then x + 1 else x - 1; in result" 2;
          functionApplication = testRoundTrip "let f = x: x * 2; result = f 5; in result" 10;
          listOperations = testRoundTrip "let xs = [1 2]; ys = [3 4]; result = xs ++ ys; in result" [1 2 3 4];
          attrOperations = testRoundTrip "let a = {x = 1;}; b = {y = 2;}; result = a // b; in result" {x = 1; y = 2;};
        };
        edgeCases = {
          complexBody = testRoundTrip "let x = 1; in {a = x; b = x + 1; c = [x (x + 1)];}" {a = 1; b = 2; c = [1 2];};
          emptyLet = testRoundTrip "let in 42" 42;
          functionInLet = testRoundTrip "let f = (x: y: x + y); in f 1 2" 3;
          letInFunction = testRoundTrip "(x: let y = x + 1; in y * 2) 3" 8;
          nestedAccess = testRoundTrip "let outer = 1; in let inner = outer + 1; final = inner + outer; in final" 3;
          unusedBinding = testRoundTrip "let x = 1; y = 2; in x" 1;
        };
      };

      # Functions
      _11_functions = {
        returnNixLambda =
          let result = evalAST "(x: builtins.add) {}";
          in expect.eq (result.fmap (f: f 40 2)) 42;
        applyNixLambda = testRoundTrip "(x: builtins.add) {} 40 2" 42;
        returnEvaluatedLambda =
          let result = evalAST "(x: x + 2)";
          in expect.eq ((result.fmap (f: f 40)).right or null) 42;
        applyEvaluatedLambda = testRoundTrip "(x: x + 2) 40" 42;
        applyEvaluatedLambdaNested = testRoundTrip "(x: y: x + y) 40 2" 42;
        returnEvaluatedLambdaNestedMixedApplication =
          let result = evalAST "(x: y: x + y) 40";
          in expect.eq ((result.fmap (f: f 2)).right or null) 42;
        identity = testRoundTrip "let f = x: x; in f 42" 42;
        const = testRoundTrip "let f = x: y: x; in f 1 2" 1;
        
        # Edge case tests for function parameters and applications
        simpleParams = {
          identity = testRoundTrip "(x: x) 42" 42;
          arithmetic = testRoundTrip "(x: x + 1) 5" 6;
          multiple = testRoundTrip "(x: y: x + y) 3 4" 7;
          nested = testRoundTrip "(x: (y: x + y)) 1 2" 3;
          currying = testRoundTrip "let f = x: y: z: x + y + z; in f 1 2 3" 6;
        };
        
        attrParams = {
          simple = testRoundTrip "({a}: a) {a = 42;}" 42;
          multiple = testRoundTrip "({a, b}: a + b) {a = 1; b = 2;}" 3;
          withDefaults = testRoundTrip "({a ? 1, b}: a + b) {b = 2;}" 3;
          ellipsis = testRoundTrip "({a, ...}: a) {a = 1; b = 2;}" 1;
          defaultOverride = testRoundTrip "({a ? 1}: a) {a = 2;}" 2;
          complexDefaults = testRoundTrip "({a ? 1, b ? a + 1}: a + b) {}" 3;
          mixedParams = testRoundTrip "({a, b ? 10}: a + b) {a = 5;}" 15;
        };
        
        scopeAndClosure = {
          closure = testRoundTrip "let x = 1; f = y: x + y; in f 2" 3;
          nestedClosure = testRoundTrip "let x = 1; in (let y = 2; f = z: x + y + z; in f 3)" 6;
          shadowParameter = testRoundTrip "let x = 1; f = x: x + 1; in f 5" 6;
          recursiveReference = testRoundTrip "let f = x: if x == 0 then 1 else x * f (x - 1); in f 3" 6;
        };
        
        partialApplication = {
          curried = testRoundTrip "let add = x: y: x + y; add5 = add 5; in add5 3" 8;
          complex = testRoundTrip "let f = a: b: c: a + b * c; g = f 1; h = g 2; in h 3" 7;
          withAttrs = testRoundTrip "let f = {a}: b: a + b; g = f {a = 10;}; in g 5" 15;
        };
        
        errorCases = {
          missingParam = expectEvalError RuntimeError "({a, b}: a + b) {a = 1;}";
          unknownParam = expectEvalError RuntimeError "({a}: a) {a = 1; b = 2;}";
          wrongType = expectEvalError TypeError "({a}: a) 42";
          nestedApplication = testRoundTrip "((x: y: x + y) 1) 2" 3;
        };

        functors = {
          isFunctionBuiltins = testRoundTrip "builtins.isFunction { __functor = self: x: x + 1; }" false;
          isFunctionLib = testRoundTrip "lib.isFunction { __functor = self: x: x + 1; }" true;
          isAttrs = testRoundTrip "builtins.isAttrs { __functor = self: x: x + 1; }" true;
          callable = testRoundTrip "{ __functor = self: x: x + 1; } 1" 2;
          returnCallable =
            let result = evalAST "{ __functor = self: x: x + 1; }";
            in expect.eq ((result.fmap (f: f 1)).right or null) 2;
        };
      };

      # Attribute access
      _12_attrAccess = {
        simple = {
          basic = testRoundTrip "{ a = 42; }.a" 42;
          letIn = testRoundTrip "let xs = { a = 42; }; in xs.a" 42;
          nested = testRoundTrip "{ a = { b = 42; }; }.a.b" 42;
          deepNesting = testRoundTrip "{ a = { b = { c = { d = 42; }; }; }; }.a.b.c.d" 42;
          multipleAttrs = testRoundTrip "{ a = 1; b = 2; }.a + { a = 1; b = 2; }.b" 3;
        };
        
        withDefaults = {
          exists = testRoundTrip "{ a = 42; }.a or 0" 42;
          missing = testRoundTrip "{ a = 42; }.b or 0" 0;
          nestedExists = testRoundTrip "{ a = { b = 42; }; }.a.b or 0" 42;
          nestedMissing = testRoundTrip "{ a = { b = 42; }; }.a.c or 0" 0;
          chainedDefaults = testRoundTrip "{ a = 1; }.b or { c = 2; }.c or 3" 2;
          complexDefault = testRoundTrip "{ a = 1; }.b or (1 + 2)" 3;
          nullDefault = testRoundTrip "{ a = 1; }.b or null" null;
          stringDefault = testRoundTrip ''{ a = 1; }.b or "default"'' "default";
        };
        
        dynamicAccess = {
          stringKey = testRoundTrip ''{ "hello world" = 42; }."hello world"'' 42;
          numberKey = testRoundTrip ''{ "123" = 42; }."123"'' 42;
          specialChars = testRoundTrip ''{ "@#$" = 42; }."@#$"'' 42;
          computed = testRoundTrip ''let key = "a"; attrs = { a = 42; }; in attrs.a'' 42;
          expression = testRoundTrip ''let attr = if true then "a" else "b"; in { a = 42; }.a'' 42;
        };
        
        errorCases = {
          missingAttr = expectEvalError MissingAttributeError "{ a = 1; }.b";
          missingNested = expectEvalError MissingAttributeError "{ a = { b = 1; }; }.a.c";
          accessNonAttr = expectEvalError TypeError "42.a";
          accessNull = expectEvalError TypeError "null.a";
          accessString = expectEvalError TypeError ''"hello".a'';
          accessList = expectEvalError TypeError "[1 2 3].a";
          deepMissing = expectEvalError MissingAttributeError "{ a = { b = 1; }; }.a.b.c";
        };
        
        expressions = {
          arithmetic = testRoundTrip "{ a = 1; b = 2; }.a + { a = 1; b = 2; }.b" 3;
          comparison = testRoundTrip "{ a = 1; }.a == 1" true;
          conditional = testRoundTrip "if { test = true; }.test then 1 else 2" 1;
          function = testRoundTrip "{ f = x: x + 1; }.f 5" 6;
          list = testRoundTrip "{ items = [1 2 3]; }.items" [1 2 3];
          nestedFunction = testRoundTrip "{ outer = { inner = x: x * 2; }; }.outer.inner 5" 10;
        };
        
        specialCases = {
          emptyAttr = testRoundTrip "{}.a or 42" 42;
          nullValue = testRoundTrip "{ a = null; }.a" null;
          boolValue = testRoundTrip "{ flag = true; }.flag" true;
          listValue = testRoundTrip "{ items = []; }.items" [];
          attrValue = testRoundTrip "{ nested = {}; }.nested" {};
          functionValue = testRoundTrip "{ id = x: x; }.id 42" 42;
        };
        
        chaining = {
          simple = testRoundTrip "{ a = { b = { c = 42; }; }; }.a.b.c" 42;
          withDefaults = testRoundTrip "{ a = { b = 1; }; }.a.c or { a = { b = 1; }; }.a.b" 1;
          mixed = testRoundTrip "{ a = { b = 2; }; }.a.b + { x = { y = 3; }; }.x.y" 5;
          conditional = testRoundTrip "{ test = { flag = true; }; }.test.flag && true" true;
        };
        
        contextual = {
          inLet = testRoundTrip "let obj = { a = 1; }; in obj.a" 1;
          inFunction = testRoundTrip "(obj: obj.value) { value = 42; }" 42;
          inList = testRoundTrip "[{ a = 1; }.a { b = 2; }.b]" [1 2];
          inAttr = testRoundTrip "{ result = { a = 1; }.a; }" { result = 1; };
          recursive = testRoundTrip "rec { a = b.value; b = { value = 42; }; }.a" 42;
        };
        
        edgeCases = {
          #selfReference = testRoundTrip "let obj = { self = obj; value = 42; }; in obj.self.value" 42;
          multipleChains = testRoundTrip "{ a = { x = 1; }; b = { y = 2; }; }.a.x + { a = { x = 1; }; b = { y = 2; }; }.b.y" 3;
          withArithmetic = testRoundTrip "{ value = 10; }.value / 2" 5;
          withLogical = testRoundTrip "{ flag = true; }.flag && false" false;
          complexNesting = testRoundTrip "{ a = { b = { c = { d = { e = 42; }; }; }; }; }.a.b.c.d.e" 42;
        };
      };

      # Assert expressions - testing proper Nix semantics
      _13_assertExpressions = {
        basic = {
          assertTrue = testRoundTrip "assert true; 42" 42;
          assertFalse = expectEvalError AssertError "assert false; 42";
          withParens = testRoundTrip "assert (true); 42" 42;
          withExpression = testRoundTrip "assert (1 == 1); 42" 42;
        };
        
        typeErrors = {
          assertString = expectEvalError TypeError ''assert "error message"; 42'';
          assertInteger = expectEvalError TypeError "assert 1; 42";
          assertZero = expectEvalError TypeError "assert 0; 42";
          assertNull = expectEvalError TypeError "assert null; 42";
          assertList = expectEvalError TypeError "assert []; 42";
          assertAttrs = expectEvalError TypeError "assert {}; 42";
          assertFunction = expectEvalError TypeError "assert (x: x); 42";
        };
        
        complexConditions = {
          arithmetic = testRoundTrip "assert (1 + 1 == 2); 42" 42;
          comparison = testRoundTrip "assert (5 > 3); 42" 42;
          logical = testRoundTrip "assert (true && true); 42" 42;
          negation = testRoundTrip "assert (!false); 42" 42;
          nested = testRoundTrip "assert ((1 < 2) && (3 > 2)); 42" 42;
          withOr = testRoundTrip "assert (false || true); 42" 42;
          complex = testRoundTrip "assert (1 + 2 == 3 && 4 * 2 == 8); 42" 42;
        };
        
        failingConditions = {
          falseComparison = expectEvalError AssertError "assert (1 == 2); 42";
          failingArithmetic = expectEvalError AssertError "assert (1 + 1 == 3); 42";
          failingLogical = expectEvalError AssertError "assert (true && false); 42";
          failingNegation = expectEvalError AssertError "assert (!true); 42";
          complexFailing = expectEvalError AssertError "assert (1 > 2 || 3 < 2); 42";
        };
        
        nested = {
          simpleNested = testRoundTrip "assert true; assert true; 42" 42;
          chainedTrue = testRoundTrip "assert (1 == 1); assert (2 == 2); 42" 42;
          firstFails = expectEvalError AssertError "assert false; assert true; 42";
          secondFails = expectEvalError AssertError "assert true; assert false; 42";
          deepNesting = testRoundTrip "assert true; (assert true; (assert true; 42))" 42;
        };
        
        contextual = {
          inLet = testRoundTrip "let x = true; in assert x; 42" 42;
          letFails = expectEvalError AssertError "let x = false; in assert x; 42";
          inFunction = testRoundTrip "(x: assert x; 42) true" 42;
          functionFails = expectEvalError AssertError "(x: assert x; 42) false";
          inConditional = testRoundTrip "if true then (assert true; 42) else 0" 42;
          conditionalFails = expectEvalError AssertError "if true then (assert false; 42) else 0";
          inList = testRoundTrip "[(assert true; 42)]" [42];
          inAttrs = testRoundTrip "{ value = assert true; 42; }" { value = 42; };
        };
        
        withScope = {
          localVariable = testRoundTrip "let valid = true; in assert valid; 42" 42;
          computation = testRoundTrip "let x = 1; y = 2; in assert (x + y == 3); 42" 42;
          functionCall = testRoundTrip "let f = x: x > 0; in assert (f 5); 42" 42;
          nestedScope = testRoundTrip "let outer = true; in let inner = outer; in assert inner; 42" 42;
          shadowing = testRoundTrip "let x = true; in let x = false; in assert (!x); 42" 42;
        };
        
        edgeCases = {
          emptyBody = testRoundTrip "assert true; null" null;
          complexBody = testRoundTrip "assert true; { a = 1; b = [2 3]; }" { a = 1; b = [2 3]; };
          recursiveAssert = testRoundTrip "let f = x: if x == 0 then (assert true; 1) else (assert (x > 0); x * f (x - 1)); in f 3" 6;
          assertInRecursion = testRoundTrip "rec { value = assert (other == 42); other; other = 42; }.value" 42;
          multipleReturns = testRoundTrip "if true then (assert true; 1) else (assert true; 2)" 1;
        };
        
        propagation = {
          throughArithmetic = expectEvalError AssertError "1 + (assert false; 2)";
          throughComparison = expectEvalError AssertError "(assert false; 1) == 1";
          throughLogical = expectEvalError AssertError "true && (assert false; true)";
          throughList = expectEvalError AssertError "[1 (assert false; 2) 3]";
          throughAttrs = expectEvalError AssertError "{ a = 1; b = assert false; 2; }";
          throughFunction = expectEvalError AssertError "(x: x + 1) (assert false; 5)";
        };
      };

      # Abort expressions - testing our custom abort handling
      _14_abortExpressions = {
        basic = {
          simpleString = expectEvalError Abort ''abort "custom abort message"'';
          emptyString = expectEvalError Abort ''abort ""'';
          withEscapes = expectEvalError Abort ''abort "hello\nworld"'';
          withUnicode = expectEvalError Abort ''abort "λ error"'';
        };
        
        computed = {
          stringConcat = expectEvalError Abort ''abort ("a " + "msg")'';
          withArithmetic = expectEvalError Abort ''abort ("error " + builtins.toString 42)'';
          conditional = expectEvalError Abort ''abort (if true then "yes" else "no")'';
          fromVariable = expectEvalError Abort ''let msg = "error"; in abort msg'';
          complex = expectEvalError Abort ''abort ("prefix: " + (builtins.toString (1 + 2)))'';
        };
        
        typeErrors = {
          nonString = expectEvalError TypeError "abort 42";
          nullMessage = expectEvalError TypeError "abort null";
          boolMessage = expectEvalError TypeError "abort true";
          listMessage = expectEvalError TypeError "abort []";
          attrMessage = expectEvalError TypeError "abort {}";
          functionMessage = expectEvalError TypeError "abort (x: x)";
        };
        
        propagation = {
          throughArithmetic = expectEvalError Abort ''1 + (abort "msg")'';
          throughComparison = expectEvalError Abort ''(abort "msg") == 1'';
          throughLogical = expectEvalError Abort ''true && (abort "msg")'';
          throughList = expectEvalError Abort ''[1 (abort "msg") 3]'';
          throughAttrs = expectEvalError Abort ''{ a = 1; b = abort "msg"; }'';
          throughFunction = expectEvalError Abort ''(x: x + 1) (abort "msg")'';
          leftOperand = expectEvalError Abort ''(abort "left") + 1'';
          rightOperand = expectEvalError Abort ''1 + (abort "right")'';
        };
        
        contextual = {
          inLet = expectEvalError Abort ''let x = abort "error"; in x'';
          inConditional = expectEvalError Abort ''if true then (abort "error") else 42'';
          inElse = expectEvalError Abort ''if false then 42 else (abort "error")'';
          inCondition = expectEvalError Abort ''if (abort "error") then 1 else 2'';
          inFunction = expectEvalError Abort ''(x: abort "error") 42'';
          inApplication = expectEvalError Abort ''((abort "error") x) 42'';
          inRecursive = expectEvalError Abort ''rec { a = abort "error"; b = a; }'';
        };
        
        nested = {
          doubleAbort = expectEvalError Abort ''abort (abort "inner")'';
          abortInCondition = expectEvalError Abort ''abort (if (abort "condition") then "yes" else "no")'';
          abortInArithmetic = expectEvalError Abort ''abort (1 + (abort "inner"))'';
        };
        
        shortCircuiting = {
          logicalAndFalse = testRoundTrip ''false && (abort "should not reach")'' false;
          logicalOrTrue = testRoundTrip ''true || (abort "should not reach")'' true;
          logicalAndAbort = expectEvalError Abort ''(abort "error") && false'';
          logicalOrAbort = expectEvalError Abort ''(abort "error") || true'';
          conditionalTrue = testRoundTrip ''if true then 42 else (abort "should not reach")'' 42;
          conditionalFalse = testRoundTrip ''if false then (abort "should not reach") else 42'' 42;
        };
        
        edgeCases = {
          veryLongMessage = expectEvalError Abort ''abort ("very " + "long " + "error " + "message " + "here")'';
          emptyMessage = expectEvalError Abort ''abort ""'';
          messageWithNewlines = expectEvalError Abort ''abort "line1\nline2\nline3"'';
          messageWithTabs = expectEvalError Abort ''abort "tab\there"'';
          unicodeMessage = expectEvalError Abort ''abort "λ λ λ"'';
          numberInMessage = expectEvalError Abort ''abort ("error code: " + builtins.toString 404)'';
        };
        
        interaction = {
          beforeAssert = expectEvalError Abort ''(abort "first") && (assert false; true)'';
          afterAssert = expectEvalError AssertError ''(assert false; true) && (abort "second")'';
          withThrow = expectEvalError Abort ''let x = abort "abort"; y = throw "throw"; in x'';
          abortOverThrow = expectEvalError Abort ''(abort "abort") + (throw "throw")'';
        };
      };

      # With expressions - testing proper scope precedence
      _15_withExpressions = {
        basic = {
          simple = testRoundTrip "with { a = 1; }; a" 1;
          multipleAttrs = testRoundTrip "with { a = 1; b = 2; }; a + b" 3;
          emptyWith = testRoundTrip "with {}; 42" 42;
          withString = testRoundTrip ''with { msg = "hello"; }; msg'' "hello";
          withList = testRoundTrip "with { items = [1 2 3]; }; items" [1 2 3];
          withFunction = testRoundTrip "with { f = x: x + 1; }; f 5" 6;
        };
        
        shadowing = {
          lexicalShadows = testRoundTrip "let x = 1; in with { x = 2; }; x" 1;
          multipleLevels = testRoundTrip "let x = 1; in with { x = 2; }; let x = 3; in x" 3;
          partialShadowing = testRoundTrip "let a = 10; b = 20; in with { a = 1; c = 3; }; a + b + c" 33;
          withShadowsGlobal = testRoundTrip "with { y = 2; }; y" 2;
          nestedLexical = testRoundTrip "let x = 1; in let x = 2; in with { x = 3; }; x" 2;
          innerShadowing = testRoundTrip "with { a = 1; }; let a = 2; in a" 2;
        };
        
        nested = {
          simple = testRoundTrip "with { a = 1; }; with { b = 2; }; a + b" 3;
          overlapping = testRoundTrip "with { a = 1; b = 2; }; with { b = 3; c = 4; }; a + b + c" 8;
          deep = testRoundTrip "with { a = 1; }; with { b = 2; }; with { c = 3; }; a + b + c" 6;
          access = testRoundTrip "with { outer = { inner = 42; }; }; with outer; inner" 42;
          independent = testRoundTrip "with { x = 1; }; (with { y = 2; }; y) + x" 3;
        };
        
        fallback = {
          basic = testRoundTrip "with { y = 2; }; y" 2;
          withLet = testRoundTrip "let x = 1; in with { y = 2; }; x + y" 3;
          notFound = expectEvalError UnknownIdentifierError "with { a = 1; }; b";
          nestedFallback = testRoundTrip "with { a = { b = 1; }; }; a.b" 1;
          deepAccess = testRoundTrip "with { data = { values = [1 2 3]; }; }; data.values" [1 2 3];
        };
        
        typeErrors = {
          nonAttrSet = expectEvalError TypeError "with 42; x";
          nullWith = expectEvalError TypeError "with null; x";
          stringWith = expectEvalError TypeError ''with "hello"; x'';
          listWith = expectEvalError TypeError "with [1 2 3]; x";
          functionWith = expectEvalError TypeError "with (x: x); x";
        };
        
        expressions = {
          arithmetic = testRoundTrip "with { x = 5; y = 3; }; x + y" 8;
          attrOps = testRoundTrip "with { a = { x = 1; }; b = { y = 2; }; }; a // b" { x = 1; y = 2; };
          comparison = testRoundTrip "with { a = 1; b = 2; }; a < b" true;
          conditional = testRoundTrip "with { flag = true; }; if flag then 1 else 2" 1;
          function = testRoundTrip "with { double = x: x * 2; }; double 5" 10;
          listOps = testRoundTrip "with { xs = [1 2]; ys = [3 4]; }; xs ++ ys" [1 2 3 4];
        };
        
        scoping = {
          recursive = testRoundTrip "with rec { a = 1; b = a + 1; }; b" 2;
          mutualRecursion = testRoundTrip "with rec { a = b + 1; b = 5; }; a" 6;
          selfReference = testRoundTrip "with rec { factorial = n: if n == 0 then 1 else n * factorial (n - 1); }; factorial 4" 24;
          crossReference = testRoundTrip "with { x = y + 1; y = 5; }; x" 6;
        };
        
        contextual = {
          inLet = testRoundTrip "let result = with { a = 1; }; a; in result" 1;
          inFunction = testRoundTrip "(env: with env; value) { value = 42; }" 42;
          inList = testRoundTrip "with { x = 1; y = 2; }; [x y]" [1 2];
          inAttrs = testRoundTrip "with { val = 42; }; { result = val; }" { result = 42; };
          inConditional = testRoundTrip "with { test = true; }; if test then 1 else 2" 1;
          inApplication = testRoundTrip "with { f = x: x + 1; arg = 5; }; f arg" 6;
        };
        
        edgeCases = {
          emptyAttrs = testRoundTrip "with {}; 42" 42;
          withSelf = testRoundTrip "let env = { a = 1; }; in with env; a" 1;
          complexNesting = testRoundTrip "with { a = with { x = 1; }; x + 1; }; a" 2;
          # dynamicAccess = testRoundTrip ''with { "hello world" = 42; }; ${"hello world"}'' 42; // Disabled: uses string interpolation
          dynamicAccess = testRoundTrip ''with { hello = 42; }; hello'' 42;
          nullValues = testRoundTrip "with { a = null; }; a" null;
          boolValues = testRoundTrip "with { flag = false; }; flag" false;
          listValues = testRoundTrip "with { items = []; }; items" [];
          attrValues = testRoundTrip "with { nested = {}; }; nested" {};
        };
        
        interaction = {
          withLet = testRoundTrip "with { a = 1; }; let b = 2; in a + b" 3;
          withAssert = testRoundTrip "with { valid = true; }; assert valid; 42" 42;
          withAssertFail = expectEvalError AssertError "with { valid = false; }; assert valid; 42";
          withAbort = expectEvalError Abort ''with { msg = "error"; }; abort msg'';
          withConditional = testRoundTrip "with { test = true; val = 42; }; if test then val else 0" 42;
        };
      };

      # Import expressions - basic import tests
      _16_importExpressions = {
        # Import self test (simplified)
        importSelf = testRoundTrip "let path = ./default.nix; in true" true;
        paths = {
          nixpkgs = testRoundTrip "<nixpkgs>" <nixpkgs>;
          nixpkgsLib = testRoundTrip "<nixpkgs/lib>" <nixpkgs/lib>;
        };
        importNixpkgs = testRoundTrip "(import <nixpkgs> {}).lib.isBool true" true;
        importNixpkgsLib = testRoundTrip "(import <nixpkgs/lib>).isBool true" true;
        importNixpkgsLibVersion =
          expect.noLambdasEq 
            ((evalAST "(import <nixpkgs/lib>).version").fmap isString)
            ((Either EvalError "bool").pure true);
      };

      # String interpolation tests
      _17_stringInterpolation = skip {
        basic = {
          simple = testRoundTrip ''"hello ''${toString 42}"'' "hello 42";
          multiple = testRoundTrip ''"''${toString 1} + ''${toString 2} = ''${toString 3}"'' "1 + 2 = 3";
          withSpaces = testRoundTrip ''"value: ''${ toString 42 }"'' "value: 42";
          emptyString = testRoundTrip ''"''${toString 0}"'' "0";
          onlyInterpolation = testRoundTrip ''''${toString 42}'' "42";
          quotesInside = testRoundTrip ''"value: ''${toString "quoted"}"'' "value: quoted";
          newlinesInside = testRoundTrip ''"text: ''${toString "line1\nline2"}"'' "text: line1\nline2";
          backslashInside = testRoundTrip ''"path: ''${toString "a\\b"}"'' "path: a\\b";
        };
        
        expressions = {
          arithmetic = testRoundTrip ''"result: ''${toString (1 + 2)}"'' "result: 3";
          comparison = testRoundTrip ''"test: ''${toString (1 == 1)}"'' "test: true";
          conditional = testRoundTrip ''"value: ''${toString (if true then 42 else 0)}"'' "value: 42";
          functionCall = testRoundTrip ''"doubled: ''${toString ((x: x * 2) 5)}"'' "doubled: 10";
          listAccess = testRoundTrip ''"first: ''${toString (builtins.head [1 2 3])}"'' "first: 1";
          attrAccess = testRoundTrip ''"value: ''${toString {a = 42;}.a}"'' "value: 42";
        };
        
        nested = {
          simple = testRoundTrip ''"outer ''${let x = "inner"; in x} end"'' "outer inner end";
          withInterpolation = testRoundTrip ''"a ''${"b ''${toString 1} c"} d"'' "a b 1 c d";
          deepNesting = testRoundTrip ''"''${toString (1 + (let x = 2; in x + 3))}"'' "6";
          multiLevel = testRoundTrip ''"''${"level1 ''${"level2"}"}"'' "level1 level2";
        };
        
        types = {
          strings = testRoundTrip ''"hello ''${"world"}"'' "hello world";
          integers = testRoundTrip ''"number: ''${toString 42}"'' "number: 42";
          floats = testRoundTrip ''"pi: ''${toString 3.14}"'' "pi: 3.14";
          booleans = testRoundTrip ''"flag: ''${toString true}"'' "flag: true";
          nullValue = testRoundTrip ''"null: ''${toString null}"'' "null: null";
          lists = testRoundTrip ''"list: ''${toString [1 2 3]}"'' "list: [ 1 2 3 ]";
        };
        
        escapingNormal = {
          dollarEscape = testRoundTrip ''"literal \''${not interpolated}"'' "literal \${not interpolated}";
          quoteEscape = testRoundTrip ''"literal \"quotes\""'' ''literal "quotes"'';
        };

        escapingIndent = {
          dollarEscape = testRoundTrip "''literal ''\${not interpolated}''" "literal \${not interpolated}";
          quoteEscape = testRoundTrip "''literal '''quotes'''''" "''literal ''quotes''";
        };
        
        contextual = {
          inLet = testRoundTrip ''let msg = "hello"; in "''${msg} world"'' "hello world";
          inFunction = testRoundTrip ''(name: "Hello ''${name}!") "Alice"'' "Hello Alice!";
          inList = testRoundTrip ''["''${toString 1}" "''${toString 2}"]'' ["1" "2"];
          inAttrs = testRoundTrip ''{msg = "value: ''${toString 42}";}.msg'' "value: 42";
          inConditional = testRoundTrip ''if true then "yes: ''${toString 1}" else "no"'' "yes: 1";
        };
        
        withScope = {
          localVariable = testRoundTrip ''let x = 42; in "value: ''${toString x}"'' "value: 42";
          computation = testRoundTrip ''let x = 1; y = 2; in "''${toString x} + ''${toString y} = ''${toString (x + y)}"'' "1 + 2 = 3";
          function = testRoundTrip ''let f = x: x * 2; in "doubled: ''${toString (f 5)}"'' "doubled: 10";
          recursive = testRoundTrip ''rec { msg = "value: ''${toString value}"; value = 42; }.msg'' "value: 42";
        };
        
        edgeCases = {
          emptyInterpolation = testRoundTrip ''"''${toString null}"'' "null";
          multipleEmpty = testRoundTrip ''"''${toString null}''${toString null}"'' "nullnull";
          onlyInterpolations = testRoundTrip ''''${toString 1}''${toString 2}'' "12";
          longString = testRoundTrip ''"very long string with ''${toString 42} in the middle"'' "very long string with 42 in the middle";
          manyInterpolations = testRoundTrip ''"''${toString 1}''${toString 2}''${toString 3}''${toString 4}"'' "1234";
          withIndentation = testRoundTrip "''value: \${toString 42}''" "value: 42";
        };
      };

      # Operator precedence and associativity tests
      _18_operatorPrecedence = {
        arithmetic = {
          additionMultiplication = testRoundTrip "1 + 2 * 3" 7;
          multiplicationAddition = testRoundTrip "2 * 3 + 1" 7;
          divisionAddition = testRoundTrip "8 / 2 + 1" 5;
          additionDivision = testRoundTrip "1 + 8 / 2" 5;
          subtraction = testRoundTrip "10 - 3 - 2" 5;
          division = testRoundTrip "24 / 4 / 2" 3;
          mixed = testRoundTrip "1 + 2 * 3 - 4 / 2" 5;
          parentheses = testRoundTrip "(1 + 2) * (3 - 1)" 6;
          deepNesting = testRoundTrip "((1 + 2) * 3) + ((4 - 1) * 2)" 15;
        };
        
        comparison = {
          arithmeticComparison = testRoundTrip "1 + 2 == 3" true;
          comparisonArithmetic = testRoundTrip "3 == 1 + 2" true;
          multiple = testRoundTrip "1 < 2 && 3 > 2" true;
          nested = testRoundTrip "(1 + 1) < (2 + 2)" true;
          withNegation = testRoundTrip "!(1 > 2)" true;
          chained = testRoundTrip "(1 < 2) && (2 < 3)" true;
        };
        
        logical = {
          andOr = testRoundTrip "true && false || true" true;
          orAnd = testRoundTrip "false || true && false" false;
          notAnd = testRoundTrip "!true && false" false;
          notOr = testRoundTrip "!false || false" true;
          parentheses = testRoundTrip "(true && false) || true" true;
          complex = testRoundTrip "true && (false || true) && true" true;
          withComparison = testRoundTrip "1 == 1 && 2 != 3" true;
          mixedTypes = testRoundTrip "(1 < 2) && (3 > 2) || false" true;
        };
        
        unary = {
          negationArithmetic = testRoundTrip "-1 + 2" 1;
          arithmeticNegation = testRoundTrip "1 + (-2)" (-1);
          notComparison = testRoundTrip "!(1 == 2)" true;
          doubleNegation = testRoundTrip "--1" 1;
          notNot = testRoundTrip "!!true" true;
          mixed = testRoundTrip "!false && (-1) < 0" true;
          parentheses = testRoundTrip "-(1 + 2)" (-3);
          complex = testRoundTrip "!(-1 > 0)" true;
        };
        
        attributeAccess = {
          beforeArithmetic = testRoundTrip "{a = 2;}.a * 3" 6;
          afterArithmetic = testRoundTrip "2 * {a = 3;}.a" 6;
          withComparison = testRoundTrip "{a = 1;}.a == 1" true;
          nested = testRoundTrip "{a = {b = 2;};}.a.b + 1" 3;
          withFunction = testRoundTrip "{f = x: x + 1;}.f 5" 6;
          chained = testRoundTrip "{a = {b = {c = 42;};};}.a.b.c" 42;
          withOr = testRoundTrip "{a = 1;}.b or 2" 2;
          complexOr = testRoundTrip "{a = 1;}.b or {c = 2;}.c" 2;
        };
        
        application = {
          beforeArithmetic = testRoundTrip "(x: x + 1) 2 * 3" 9;
          afterArithmetic = testRoundTrip "2 * (x: x + 1) 3" 8;
          nested = testRoundTrip "(x: y: x + y) 1 2 + 3" 6;
          withComparison = testRoundTrip "(x: x > 0) 5 && true" true;
          complex = testRoundTrip "(f: x: f (f x)) (y: y + 1) 0" 2;
        };
        
        listOperations = {
          concat = testRoundTrip "[1] ++ [2] ++ [3]" [1 2 3];
          concatArithmetic = testRoundTrip "[(1 + 1)] ++ [(2 * 2)]" [2 4];
          concatComparison = testRoundTrip "[(1 == 1)] ++ [(2 != 3)]" [true true];
          nested = testRoundTrip "[([1] ++ [2])] ++ [[3]]" [[1 2] [3]];
        };
        
        attrOperations = {
          merge = testRoundTrip "{a = 1;} // {b = 2;} // {c = 3;}" {a = 1; b = 2; c = 3;};
          mergeArithmetic = testRoundTrip "{a = 1 + 1;} // {b = 2 * 2;}" {a = 2; b = 4;};
          mergeComparison = testRoundTrip "{a = 1 == 1;} // {b = 2 != 3;}" {a = true; b = true;};
          precedence = expectEvalError TypeError "{a = 1;} // {b = 2;}.b == 2";
        };
        
        comprehensive = {
          everything = testRoundTrip "!false && 1 + 2 * 3 == 7 || [1] ++ [2] == [1 2]" true;
          withAttrs = testRoundTrip "let x = {a = 1; b = 2;}; in x.a + x.b * 2 == 5" true;
          withFunctions = testRoundTrip "(x: y: x + y * 2) 1 3 == 7" true;
          complex = testRoundTrip "let f = x: x * 2; attrs = {val = 3;}; in f attrs.val + 1 == 7" true;
          deepNesting = testRoundTrip "((1 + 2) * (3 + 4)) == ((2 * 3) + (4 * 5) - 3)" false;
        };
      };

      # Complex expressions demonstrating code transformations
      _19_transformations = let
        # Example: transform "1 + 2" to "2 + 1" (commutativity)
        original = parse "1 + 2";
        transformed = original.mapNode (node: with node; { 
          op = "-";
          lhs = rhs;
          rhs = lhs;
        });
      in {
        original = testRoundTrip original 3;
        transformed = testRoundTrip transformed 1;
      };

      # Self-eval test (skipped due to slow parsing of large file)
      _20_selfParsing = skip {
        parseParserFile =
          expect.True ((evalAST (builtins.readFile ./ast.nix)) ? right);
      };
    };

  };
}
