{ lib, collective-lib, nix-reflect, ... }:

let 
  inherit (nix-reflect) parser debuglib eval;
  inherit (parser) parse;
  inherit (collective-lib.typed.script-utils.ansi-utils) ansi;
in 
  with collective-lib.typed;
rec {
  # Do not move; predictable source location for reflection tests
  testData = {
    minimalDoBlock =
      (Eval.do
        {a = Eval.pure "pure string";}
        ({a, _}: _.pure a));

    doBlock =
      (Eval.do
        {a = Eval.pure "independent bind";}
        {b = pure "implicit dependent bind";}
        {c = {_}: _.pure "implicit dependent bind";}
        (Eval.pure "independent action")
        (pure "implicit dependent action")
        ({_}: _.pure "explicit dependent action")
        (Eval.do
          {a = Eval.pure "nested independent bind";}
          {b = pure "nested implicit dependent bind";}
          {c = {_}: _.pure "nested implicit dependent bind";}
          (Eval.pure "nested independent action")
          (pure "nested implicit dependent action")
          ({_}: _.pure "nested explicit dependent action")));
  };

  checkTypes = all (T: T ? check || isbuiltinName T);
    
  assertIs = T: a:
    if T ? check then 
      assert assertMsg (T.check a) (_b_ ''
        check: expected value of type ${T}, got: ${getT a}
      ''); 
      true
    else if isbuiltinName T then
      assert assertMsg (typeOf a == T) (_b_ ''
        check: expected value of type ${T}, got: ${getT a}
      ''); 
      true
    else
      assert assertMsg false (_b_ ''
        check: expected type string or __type ${T}, got: ${lib.typeOf T}
      ''); 
      true;

  is = T: a: 
    if T ? check then T.check a
    else isbuiltinName T && typeOf a == T;

  tEq = a: b: pointerEqual a b || toString a == toString b;

  # Get the type of a value.
  getT = a: a.__type or (typeOf a);

  # Get the Monad type of a value.
  getM = a: 
    assert that (isMonadValue a) ''
      getM: expected monad but got ${_p_ a}
    '';
    if isDo a then a.M
    else getT (getT a);

  Any = {
    __toString = self: "Any";
    check = x: true;
    __functor = self: value: {
      __type = Any;
      inherit value;
    };
  };

  Either = E: A: assert checkTypes [E A]; rec {
    inherit E A;
    __toString = self: "Either ${E} ${A}";
    __functor = self: x:
      assert that (is E x || is A x) ''Either: expected type ${E} or ${A} but got ${_p_ x}'';
      if is E x then Left x else Right x;
    check = x: (isLeft x && is Left x) || (isRight x && is Right x);
    Left = __Left E A;
    Right = __Right E A;
    pure = Right;
  };

  __Left = E: A: assert checkTypes [E A]; {
    __toString = self: "(Either ${E} ${A}).Left";
    check = e: e ? __isLeft && is E e.left;
    __functor = self: e:
      assert that (is E e) ''Either.Left: expected type ${E} but got ${_p_ e}'';
      let this = {
        __type = Either E A;
        __isLeft = true; 
        __toString = self: "Left ${_p_ e}";
        left = e; 
        case = case this;
        unwrap = unwrap this;
        fmap = _: this;
      }; in this;
  };

  __Right = E: A: assert checkTypes [E A]; {
    __toString = self: "(Either ${E} ${A}).Right";
    check = a: a ? __isRight && is A a.right;
    __functor = self: a:
      assert that (is A a) ''Either.Right: expected type ${A} but got ${_p_ a}'';
      let this = { 
        __type = Either E A;
        __isRight = true; 
        __toString = self: "Right ${_p_ a}"; 
        right = a; 
        case = case this;
        unwrap = unwrap this;
        fmap = f: 
          let 
            b = f a;
            B = getT b;
          in __Right E B b;
      }; in this;
  };

  isEither = x: x ? __isLeft || x ? __isRight;
  isLeft = x: x ? __isLeft;
  isRight = x: x ? __isRight;
  case = e: cases: if isLeft e then cases.Left e.left else cases.Right e.right;
  unwrap = e: e.case {
    Left = e: e;
    Right = a: a;
  };

  EvalError = rec {
    __toString = self: "EvalError";
    check = x: x ? __isEvalError;
    __functor = self: name: {
      __toString = self: "EvalError.${name}";
      __isErrorType = true;
      check = x: (x ? __isEvalError) && (x ? "__isEvalError${name}");
      __functor = self: __msg: {
        __type = EvalError;
        __errorName = name;
        __isEvalError = true; 
        "__isEvalError${name}" = true; 
        __toString = self: _b_ ''
          EvalError.${name}:
            ${_h_ __msg}
        '';
        inherit __msg;
      };
    };
  };

  isErrorType = x: x ? __isErrorType;

  Abort = EvalError "Abort";
  AssertError = EvalError "AssertError";
  Throw = EvalError "Throw";
  TypeError = EvalError "TypeError";
  RuntimeError = EvalError "RuntimeError";
  SyntaxError = EvalError "SyntaxError";
  UnknownIdentifierError = EvalError "UnknownIdentifierError";
  MissingAttributeError = EvalError "MissingAttributeError";
  CatchableMissingAttributeError = EvalError "CatchableMissingAttributeError";
  NixPathError = EvalError "NixPathError";
  MissingThunkError = EvalError "MissingThunkError";
  InvalidThunkError = EvalError "InvalidThunkError";

  isCatchableError = x:
    isEvalError x
    && (is AssertError x || is Throw x || is CatchableMissingAttributeError x);

  isEvalError = x: x ? __isEvalError;

  # Handles forcing a thunk with current state cache and writing it back
  force = x: {_}:
    if !(isThunk x)
      then _.do
        (while "not forcing a non-thunk of type ${lib.typeOf x}")
        (pure x)
      else _.do
        (while {_ = "forcing ${x} via thunk cache";})
        # Force thunks, persisting the value in the cache upon first force.
        {thunkCache = getThunkCache;}
        ({thunkCache, _}: _.bind (thunkCache.forceThunk x));

  isThunk = x: x ? __isThunk;

  mkThunk = _: thunkId: node: 
    let _self_ = _;
    in lib.fix (self: {
      __isThunk = true;
      inherit thunkId;
      inherit (node) nodeType;
      __toString = self: "<CODE#${thunkId}|${self.nodeType}|nested=${_p_ (isThunk node)}>";

      # Run the thunk fully and return the full monadic output {s, a}
      runWithCacheM = thunkCache: {_, ...}: _.do
        (while "forcing ${self} in runWithCacheM with cache:\n${thunkCache}")
        {scope = getScope;}
        {initScope = getInitScope;}
        ({scope, initScope, _}: _.saveScope (_self_.do
          (setScope initScope)
          (setThunkCache thunkCache)
          (while "forcing '${self.nodeType}' thunk with inherited cache:\n${thunkCache}")
          {a = eval.ast.evalNodeM node;}
          {s = get;}
          ({a, s, _}: _.pure {inherit a s;})));
    });

  Thunk = node:
    # Do not allow identifier node-thunks, which can then refer to themselves in an infinite loop.
    if node.nodeType == "identifier" then nix-reflect.eval.ast.evalIdentifier node
    else {_}: _.do
      (while "constructing '${node.nodeType}' Thunk")
      {thunkCache = getThunkCache;}
      {thunk = {thunkCache, _}: _.bind (thunkCache.cacheNode node);}
      ({thunk, _}: _.do 
        (while "returning constructed ${thunk}")
        (pure thunk));

  ThunkCache = {
    __toString = self: "ThunkCache";
    check = x: x ? __isThunkCache;
    __functor = self:
      {thunks ? {}, values ? {}, nextId ? 0, hits ? 0, misses ? 0}:
      (lib.fix (this: {
        __toString = self: self.print {};

        print = args: with script-utils.ansi-utils.ansi; box ({
          header = style [fg.magenta bold] "ThunkCache";
          body = ''
            ${_h_ (this.stats {})}

            ${_h_ (this.debug {})}
          '';
        } // args);

        __type = ThunkCache;
        __isThunkCache = true;
        inherit thunks values nextId hits misses;

        stats = {}: _b_ ''
          ${toString (size values)}/${toString (size thunks)} forced
          ${toString hits}/${toString (hits+misses)} hits
          next ID: ${toString nextId}
        '';
        debug = {}: _b_ ''
          thunks:
            ${_ph_ this.thunks}

          values:
            ${_ph_ this.values}
        '';

        # Cache and return the thunk that evaluates the node.
        cacheNode = node: {_}:
          let 
            thunkId = toString this.nextId;
            thunk = mkThunk _ thunkId node;
          in _.do
            (whileV 3 "writing ${thunk} into cache:\n${this}")
            (setThunkCache (ThunkCache { 
              inherit (this) values hits misses;
              thunks = this.thunks // { ${thunkId} = thunk; };
              nextId = this.nextId + 1;
            }))
            (whileV 3 "returning ${thunk} from cacheNode")
            (pure thunk);

        # Argument is just used as ID carrier.
        # TODO: ThunkCache could just hold nextId and values.
        forceThunk = thunk:
          let thunkId = thunk.thunkId;
              thunkCache = this;
          in {_}: _.do
            (while "forcing ${thunk} in forceThunk")
            (guard (thunkCache.thunks ? ${thunkId}) (MissingThunkError ''
              ThunkCache.forceThunkId: thunkId ${thunkId} not found in cache:
                ${thunkCache}
            ''))
            (let cachedThunk = thunkCache.thunks.${thunkId};
              in guard (thunk.nodeType == cachedThunk.nodeType) (InvalidThunkError ''
              ThunkCache.forceThunk:
                Provided thunk ${thunk} has nodeType ${thunk.nodeType}
                Cached thunk ${cachedThunk} has nodeType ${cachedThunk.nodeType}
            ''))
            ({_}:
              if thunkCache.values ? ${thunkId} then _.do
                (while "ThunkCache hit for ${thunk}:\n${thunkCache}")
                (setThunkCache (ThunkCache {
                  inherit (thunkCache) thunks values nextId misses;
                  hits = thunkCache.hits + 1;
                }))
                (pure thunkCache.values.${thunkId})

              else _.do
                (while "ThunkCache miss for ${thunk}:\n${thunkCache}")
                {result = thunk.runWithCacheM thunkCache;}
                ({result, _}:
                  let thunkCache' = result.s.thunkCache;
                  in _.do
                    (setThunkCache (ThunkCache {
                      inherit (thunkCache') thunks nextId hits;
                      misses = thunkCache'.misses + 1;
                      values = thunkCache'.values // { 
                        ${thunkId} = result.a;
                      };
                    }))
                    (pure result.a)));
      }));
  };

  EvalState = rec {
    __toString = self: "EvalState";
    check = x: x ? __isEvalState;
    mempty = _: initState;

    __functor = self: {scope ? initScope, thunkCache ? ThunkCache {}}: (lib.fix (this: {
      inherit scope thunkCache;
      publicScope = {}: removeAttrs scope ["__internal__"];

      __type = EvalState;
      __isEvalState = true;
      __toString = self: with script-utils.ansi-utils.ansi; box {
        header = style [fg.cyan bold] "EvalState";
        body = joinVerticalSep " " [
          (this.printScope {margin = zeros;})
          (this.thunkCache.print {margin = zeros;})
        ];
      };
      printScope = args: with script-utils.ansi-utils.ansi; box ({
        header = style [bold] "Scope";
        body = ''
          ${_ph_ (this.scope)}
        '';
      } // args);

      fmap = f: EvalState { scope = f scope; inherit (this) thunkCache; };
    }));
  };

  resetScope = scope: {_}: _.do
    {scope = getScope;}
    ({scope, _}: _.bind scope.__internal__.resetScopeM);

  getInitScope = {_}: _.do
    {scope = getScope;}
    ({scope, _}: _.bind (scope.__internal__.initScopeM));

  mkInitEvalState = scope: EvalState { scope = mkInitScope scope; };

  mkInitScope = scope: lib.fix (self: 
    scope // {
      __internal__ = {
        withScope = {};
        initScope = {}: self;
        initScopeM = {_}: _.pure self;
        resetScopeM = {_}: _.do
          (while "resetting scope")
          (bind (setScope self));
      };
    });

  testScope = scope: recursiveMergeAttrsList [ (mkInitScope initScope) scope ];

  initState = mkInitEvalState initScope;

  initScope = lib.fix (self: {
    NIX_PATH = {
      nixpkgs = <nixpkgs>;
    };
    PWD = /tmp/pwd;
    HOME = /tmp/home;

    true = true;
    false = false;
    null = null;

    # Override builtins with library versions
    builtins =
      with nix-reflect.parser; 
      with nix-reflect.eval.ast;
      removeAttrs builtins ["builtins"] // {
        # Move the recursive self-reference to an identifier to enable laziness.
        "builtins" = N.identifier "builtins";
        "throw" = N.throwExpr;
        "abort" = N.abortExpr;
        "assert" = N.assertExpr;
        # TODO: Native import
        #"import" = ...
        # Start shimming out native versions of key builtins
        #toString = (Eval.pure unit).bind (Thunk (parse "x: x.__toString x"));
      };

    # Expose same builtins on top-level as Nix
    inherit (self.builtins) "derivation" "import" "throw" "abort";

    # Expose minimal lib to avoid error blowup
    lib = {
      inherit (lib) isFunction attrValues;
      toString = self.builtins.toString;
    };
  });

  Unit = {
    __toString = self: "Unit";
    check = x: x ? __isUnit;
    __functor = self: {}: {
      __toString = self: "unit";
      __type = Unit;
      __isUnit = true;
    };
  };

  unit = Unit {};

  void = m: 
    assert that ((m ? bind) && (m ? pure)) ''
      void: expected monad but got ${getT m}
    '';
    m.bind ({_}: _.pure unit);

  isDo = x: x ? __isDo;

  isDoOf = M: xdo: isDo xdo && tEq xdo.M M;

  isDoStatement = M: xs:
    log.while ("checking if ${_p_ xs} is a do-statement of ${M}") (
    (M != null && isMonadOf M xs)
    || (isFunction xs)
    || (isSolo xs && isDoStatement M (soloValue xs))
    );

  assertIsDoStatement = doSelf: M: xs:
    assert that (isDoStatement M xs) ''
      do: expected a statement of form

        ( ${if M == null then "<monadic value>" else M Any} )

      or

        ( { bindings, ... }: ${if M == null then "<monadic value>" else M Any} )

      or

        { name = ${if M == null then "<monadic value>" else M Any}; }

      or

        { name = ({ bindings, ... }: ${if M == null then "<monadic value>" else M Any}); }

      but got ${_pv_ xs}
    '';
    true;

  withStackError = self: msg: _b_ ''
    ${msg}

    in

    ${toString self}
  '';

  unsetM = throw "do: cannot infer monadic type from lambda expression";

  unsetType = throw "do: cannot infer type from lambda expression";

  inferMonadFromStatement = dispatch {
    lambda = _: unsetM;
    set = xs:
      if isDo xs then xs.M
      else if isMonadValue xs then (getM xs)
      else if isSolo xs then flip dispatch (soloValue xs) {
        set = getM;
        lambda = _: unsetM;
      }
      else throw "do: malformed non-solo non-monad non-do set statement: ${_p_ xs}";
  };

  inferTypeFromStatement = dispatch {
    null = _: unsetType;
    lambda = _: unsetType;
    set = xs:
      if isMonadValue xs then (getT xs)
      else if isSolo xs then flip dispatch (soloValue xs) {
        set = getT;
        lambda = _: unsetType;
      }
      else throw "do: malformed non-solo non-monad non-do set statement: ${_p_ xs}";
  };

  bindStatementSignature = M: dispatch {
    lambda = _: "DependentAction";
    set = statement:
      if isMonadOf M statement then "IndependentAction"
      else switch.typeOf (soloValue statement) {
        lambda = "DependentBind";
        set = "IndependentBind";
      };
  };

  mkNormalisedDoStatement = statement: bindName: f:
    if statement ? __isNormalisedDoStatement then statement
    else {
      __isNormalisedDoStatement = true;
      inherit bindName f;
    };

  # Idempotently convert all do statements to the same form.
  # { bindName = null | string; f = { _, _a, bindings... }: statement }
  normaliseBindStatement = M: dispatch.on (bindStatementSignature M) {
    IndependentAction = statement:
      mkNormalisedDoStatement statement null ({_ ? M.pure unit, _a ? unit, ...}: statement);
    DependentAction = statement: 
      mkNormalisedDoStatement statement null (addEllipsis statement);
    IndependentBind = statement:
      mkNormalisedDoStatement statement (soloName statement) ({_ ? M.pure unit, _a ? unit, ...}: soloValue statement);
    DependentBind = statement:
      mkNormalisedDoStatement statement (soloName statement) (addEllipsis (soloValue statement));
  };

  tryPrintStatementRHS = M: f:
    let rhs = try (f (nullRequiredArgs f // {_ = M.pure unit;})) (_: "...");
    in if isString rhs then rhs else printStatement M rhs;

  printStatement = M: 
    dispatch.on (bindStatementSignature M) {
      IndependentAction = statement:
        if isDo statement then "(" + _b_ "${_h_ (printDo statement)})"
        else "(<${getT statement}>)";
      DependentAction = statement: 
        let args = builtins.functionArgs statement;
        in _b_ ''
          (${if size args > 0
              then "{${joinSep ", " (attrNames args)}, ...}"
              else "{?}"
            }: ${_h_ (tryPrintStatementRHS M statement)})
        '';
      IndependentBind = statement: _b_ ''
        ${debuglib.printPos (debuglib.pos.path statement)}
      '';
      DependentBind = statement: _b_ ''
        ${debuglib.printPos (debuglib.pos.path statement)}
      '';
    };

  printDo = self:
    let
      # Try to infer the do block position from the first self-contained statement
      # i.e. a bind statement where { k = ...; } is source-tracked
      doPos = 
        if empty self.__statements then null
        else
          let ps = filter (x: x != null) (map debuglib.pos.path self.__statements);
          in if empty ps then null else head ps;

      header =
        if doPos == null then null
        else 
          let 
            p = soloValue doPos;
          in 
            if (p ? file) && (p ? line)
            then "${builtins.baseNameOf p.file}:${toString p.line}"
            else null;

    in 
      if empty self.__statements then "<empty do-block>"
      else _b_ ''
        ${self.M}.do${optionalString (header != null) " <${header}>"}
          ${_h_ (joinLines (map (printStatement self.M) self.__statements))}
      '';

  tryApplyDoBinding = _: doSelf: statement: f: bindings:
    errors.try (let x = tryApply f bindings; in seq x x) (_e: _.throws (SyntaxError ''
      Failed to bind:
        ${printStatement doSelf.M statement}

      In do-block:
        ${doSelf}
      
      Against bindings:
        ${_ph_ bindings}
    ''));

  tryApplyBinding = _: M: statement: f: bindings:
    errors.try (let x = tryApply f bindings; in seq x x) (_e: _.throws (SyntaxError ''
      Failed to bind:
        ${printStatement M statement}
      
      Against bindings:
        ${_ph_ bindings}
    ''));

  # Given a monad M, a state containing an M bindings and an M m monadic action,
  # and a statement of one of these forms:
  #
  # <M value>
  # { bindings, ... }: <M value>
  # { nameToBind = <M value>; }
  # { nameToBind = { bindings, ... }: <M value>; }
  #
  # Return an updated state with the bindings updated inside the monad to any new
  # bindings, and an updated monadic action.
  handleBindStatement = doSelf: M: acc: statement:
    assert (assertIsDoStatement doSelf M statement);
    let normalised = normaliseBindStatement M statement;
    in
      (acc.m.bind ({_, _a}:
        let
          mb_ = 
            # After upgrading addEllipsis to use tryApply, this will return a
            # catchable error for some syntactic-type errors in do usage like
            # binding non-existing variables.
            let bindings = acc.bindings // { inherit _ _a; };
            in tryApplyDoBinding _ doSelf statement normalised.f bindings;
         mb = if isDo mb_ then mb_.__setInitM acc.m else mb_;
        in
          assert that (isMonadOf M mb) ''
            handleBindStatement: non-${M} value returned of type ${getT mb}:
              ${_ph_ mb}

            In do-block:
              ${_ph_ doSelf}
          '';
          mb.bind ({_, _a}: _.pure {
            bindings = acc.bindings // optionalAttrs (normalised.bindName != null) {
              ${normalised.bindName} = _a;
            };
            canBind = normalised.bindName == null;
            m = mb;
          })));

  # Do-notation functor that simply stores the statements given to it
  # When bound, it runs the statements in order to produce the monadic value
  # on which to call bind.
  mkDo = M: __initM: __statements:
    log.while ("constructing a do-block of\n${joinLines (map (printStatement M) __statements)}") (
    let this = {
      __isDo = true;
      __isMonad = true;
      __type = inferTypeFromStatement (maybeLast __statements);
      inherit M __initM __statements;

      __toString = printDo;

      __functor = self: statement:
        log.while ("lazily storing a do-statement:\n${printStatement M statement}") (
        mkDo M self.__initM (self.__statements ++ [statement])
        );

      __setInitM = initM: mkDo this.M initM this.__statements;

      # Bind with specified initial monadic value.
      # Just creates a new do with self as the initial value of the last one.
      bind = statement:
        log.while ("binding a do-statement:\n${printStatement M statement}") (
        M.do
          ({_}: _.bind (this.action.bind statement))
        );

      sq = b: this.bind (_: b);

      # Actually force the block to evaluate down to a final M value.
      force = {}:
        #log.while "forcing do-block:\n${this}" (
        log.while "forcing do-block" (
        let 
          initAcc = {
            bindings = {};
            canBind = false;
            m = this.__initM;
          };
          accM = (M.pure unit).foldM (handleBindStatement this M) initAcc (this.__statements);
        in
          accM.bind ({_, _a}:
            assert that _a.canBind (withStackError this ''
              do: final statement of a do-block cannot be an assignment.
            '');
            _a.m)
        );

      action = this.force {};
      run = arg: this.action.run arg;
      run_ = arg: this.action.run_ arg;
      runEmpty = arg: this.action.runEmpty arg;
      runEmptyM = arg: this.action.runEmptyM arg;
      runEmpty_ = arg: this.action.runEmpty_ arg;
      runClosure = arg: this.action.runClosure arg;
      runClosureM = arg: this.action.runClosureM arg;
      runWith = arg: this.action.runWith arg;
      runWithM = arg: this.action.runWithM arg;
      runM = arg: this.action.runM arg;
      mapState = arg: this.action.mapState arg;
      setState = arg: this.action.setState arg;
      mapEither = arg: this.action.mapEither arg;
      catch = arg: this.action.catch arg;
      do = mkDo M this.action [];
    };
    in this
    );

  pure = x: {_, ...}: _.pure x;
  throws = e: {_, ...}: _.throws e;
  guard = cond: e: {_, ...}: _.guard cond e;
  while = msg: {_, ...}: _.while msg;
  whileV = v: msg: {_, ...}: _.whileV v msg;
  when = cond: m: {_, ...}: _.when cond m;
  unless = cond: m: {_, ...}: _.unless cond m;

  # Check if a value is a monad.
  # i.e. isMonad (Eval.pure 1) -> true
  #      isMonad (Either.pure 1) -> true
  #      isMonad (do (Eval.pure 1)) -> true
  isMonadValue = x: isDo x || (getT x) ? __isMonad;

  # Is the given x an instance of the given monad, ignoring the type parameter?
  # i.e. isMonadOf Eval (Eval.pure 1) -> true
  #      isMonadOf Eval (Either.pure 1) -> false
  isMonadOf = M: x: isMonadValue x && tEq M (getM x);

  # Monadic evaluation state.
  # Roughly simulates an ExceptT EvalError (StateT EvalState m) a monad stack.
  Eval = rec {
    __toString = self: "Eval";
    check = x: x ? __isEval;
    Error = EvalError;
    S = EvalState;
    do = mkDo Eval (Eval.pure unit) [];
    pure = x: 
      let A = getT x;
      in Eval A id ((Either Error A).pure x);
    throws = e: (Eval.pure unit).throws e;

    __functor = self: A: assert checkTypes [A]; rec {
      __toString = self: "Eval ${A}";
      __isMonad = true;
      __type = Eval;
      inherit A;
      E = Either Error A;
      check = x: x ? __isEval && is E x.e;
      pure = x: Eval A id ((Either Error A).pure x);

      __functor = self:
        s: assert that (lib.isFunction s) ''Eval: expected lambda state but got ${_p_ s}'';
        e: assert that (is E e) ''
          Eval: expected Either value ${E} but got ${getT e}:
            ${_pv_ e}
        '';

        let 
          rebind = 
            this: 
            {A ? this.A, E ? Either Error A, s ? this.s, e ? this.e, __type ? Eval A} @ args: 
              fix (this_: 
                this 
                // args 
                // mapAttrs (_: f: f this_) this.__unbound
              );
          set_s = this: s: rebind this { inherit s; };
          set_e = this: e: e.case {
            Left = e: set_e_Left this e;
            Right = a: set_e_Right this a;
          };
          set_e_Left = this: e_: rebind this (rec { A = Unit; E = Either Error Unit; e = E.Left e_;});
          set_e_Right = this: a_: rebind this (rec { A = getT a_; E = Either Error A; e = E.Right a_;});

          this = {
            __type = Eval A;
            __isEval = true;
            __toString = self: _b_ "Eval ${A} (${_ph_ self.e})";

            inherit S E A s e;

            __unbound = {
              # modify :: (EvalState -> EvalState) -> Eval A -> Eval {}
              modify = this: f: 
                if isLeft this.e then this else
                void (this.mapState (compose f));

              set = this: state: 
                if isLeft this.e then this else
                void (this.setState (const state));

              get = this: {_, ...}: _.pure (this.s (S.mempty {}));

              strictState = false;
              setState = this: s:
                let state = if this.strictState then const (s (S.mempty {})) else s;
                in set_s this s;
              mapState = this: f: this.setState (f this.s);

              setEither = this: e: set_e this e;
              mapEither = this: f: this.setEither (f this.e);
              liftEither = this: e: if isEvalError e then this.throws e else this.pure e;

              getThunkCache = this: this.bind getThunkCache;
              setThunkCache = this: this.bind setThunkCache;
              getScope = this: this.bind getScope;
              getPublicScope = this: this.bind getPublicScope;
              setScope = this: newScope: this.bind (setScope newScope);
              setPublicScope = this: newScope: this.bind (setPublicScope newScope);
              modifyPublicScope = this: f: this.bind (modifyPublicScope f);
              saveScope = this: f: this.bind (saveScope f);
              modifyScope = this: f: this.bind (modifyScope f);
              prependScope = this: newScope: this.bind (prependScope newScope);
              appendScope = this: newScope: this.bind (appendScope newScope);

              getWithScope = this: this.bind getWithScope;
              setWithScope = this: newScope: this.bind (setWithScope newScope);
              appendWithScope = this: newScope: this.bind (appendWithScope newScope);

              do = this: statement: mkDo Eval this [] statement;
              pure = this: x: set_e_Right this x;
              fmap = this: f: set_e this (this.e.fmap f);
              liftA2 = this: f: aM: bM:
                this.do
                  {a = aM;}
                  {b = bM;}
                  ({a, b, _}: _.pure (f a b));

              when = this: cond: m: if cond then this.bind m else this.pure unit;
              unless = this: cond: m: if !cond then this.bind m else this.pure unit;
              # Supports extra source printing info via while {_ = "...";}, or just while "..."
              whileV = this: v: s_:
                let s = 
                  if isAttrs s_ then _b_ (with ansi; ''
                    ${style [fg.grey] "@"} ${
                      let p = (debuglib.pos.file s_)._;
                      in "${atom.path p.file}:${atom.number (toString p.line)}:${atom.number (toString p.column)}"}
                        ${_h_ ("↳ │ " + (_ls_ (mapTailLines (line: "  │ ${line}") (s_._))))}
                  '')
                  else s_;
                # Add the stack logging to the monadic value itself
                in log.while s (
                  # Add runtime tracing to the resolution of the bind only
                  this.bind (_: (log.v v).show "while ${s}" this)
                );
              while = this: s: this.whileV 3 s;
              guard = this: cond: e: 
                if cond 
                then this.bind ({_}: _.pure unit) 
                else (this.throws e);

              foldM = this: f: initAcc: xs:
                let startM = this.pure initAcc;
                in fold.left (accM: a: accM.bind ({_, _a}: _.bind (f _a a))) startM xs;

              # traverse :: (a -> Eval b) -> [a] -> Eval [b]
              traverse = this: f: xs:
                fold.left (accM: x: accM.bind ({_, _a, ...}: _.do
                  {value = f x;}
                  ({_, value}: _.pure (_a ++ [value]))))
                  (this.pure [])
                  xs;

              bind = this: statement: 
                this.e.case {
                  Left = _: this;
                  Right = a: 
                    let normalised = normaliseBindStatement Eval statement;
                        mb = tryApplyBinding this Eval statement normalised.f {_ = this; _a = a;};
                    in assert that (isMonadOf Eval mb) ''
                      Eval.bind: non-Eval value returned of type ${getT mb}:
                        ${_ph_ mb}
                    '';
                    mb.mapState (s: compose s this.s);
                };

              sq = this: b: this.bind (_: b);

              # Set the value to the given error.
              throws = this:
                e: assert that (is Error e) ''Eval.throws: expected Either value ${Error} but got ${_p_ e} of type ${getT e}'';
                this.setEither (E.Left e);

              # Catch specific error types and handle them with a recovery function
              # catch :: (EvalError -> Eval A) -> Eval A
              catch = this: handler:
                if isLeft this.e && isCatchableError this.e.left then 
                  (set_e_Right this unit)
                  .bind (handler this.e.left)
                else this;

              # Returns (Either EvalError { a :: A, s :: S })
              run = this: initialState: 
                this.e.fmap (a: { s = this.s initialState; inherit a; });
              run_ = this: _: this.e;
              runEmpty = this: _: (this.run (S.mempty {}));
              runEmptyM = this: {_, ...}: _.pure (this.run (S.mempty {}));
              runEmpty_ = this: _: ((this.run (S.mempty {})).fmap (r: r.a));
              runClosure = this: _: (this.runEmpty_ {}).unwrap;
              runClosureM = this: {_, ...}: _.pure (this.runClosure {});
              runWith = this: scope: thunkCache: this.run (EvalState { inherit scope thunkCache; });
              runWithM = this: scope: thunkCache: {_}: _.pure (this.runWith scope thunkCache);
            };
          };
        # Bind 'this'
        in rebind this {};
    };
  };

  runWith = scope: thunkCache: closure:
    (closure.runWith scope thunkCache);

  runWithM = scope: thunkCache: closure:
    pure (runWith scope thunkCache closure);

  runM = closure:
    saveScope ({_}: _.do (closure));

  liftA2 = f: aM: bM: {_, ...}: _.liftA2 f aM bM;
  foldM = f: initAcc: xs: {_, ...}: _.foldM f initAcc xs;
  traverse = f: xs: {_, ...}: _.traverse f xs;

  get = {_, ...}: _.bind _.get;
  set = state: {_, ...}: _.set state;
  modify = f: {_, ...}: _.modify f;
  liftEither = e: {_, ...}: _.liftEither e;

  getThunkCache = {_, ...}:
    _.do
      (while "getting thunk cache")
      {state = get;}
      ({_, state}: _.pure state.thunkCache);

  setThunkCache = thunkCache: {_, ...}:
    _.do
      (while "setting thunk cache:\n${thunkCache}")
      {state = get;}
      ({_, state}: _.set (EvalState {inherit (state) scope; inherit thunkCache;}));

  saveScope = f: {_, ...}:
    _.do
      (while "with saved scope")
      {scope = getScope;}
      {a = f;}
      ({_, scope, a, ...}: _.do
        (setScope scope)
        (pure a));

  setScope = scope: {_, ...}:
    _.do
      (while "setting scope")
      {state = get;}
      ({state, _}: _.set (EvalState {inherit scope; inherit (state) thunkCache;}));

  guardScopeUpdate = scope: {_, ...}: _.do
    (guard (!(scope ? __internal__)) (RuntimeError ''
      scope must not contain __internal__:
        ${_ph_ scope}
    ''));

  setPublicScope = scope: {_, ...}:
    _.do
      (while "setting public scope")
      (guardScopeUpdate scope)
      {state = get;}
      ({state, _}: _.set (EvalState {
        scope = scope // {inherit (state.scope) __internal__;};
        inherit (state) thunkCache;
      }));
  
  modifyScope = f: {_, ...}:
    _.do
      (while "modifying scope")
      (modify (s: s.fmap f));

  modifyPublicScope = f: {_, ...}:
    _.do
      (while "modifying public scope")
      (modifyScope (scope: f scope // {inherit (scope) __internal__;}));

  getScope = {_, ...}:
    _.do
      (while "getting scope")
      {state = get;}
      ({_, state}: _.pure state.scope);

  getPublicScope = {_, ...}:
    _.do
      (while "getting public scope")
      {state = get;}
      ({_, state}: _.pure (state.publicScope {}));

  prependScope = newScope: {_, ...}:
    _.do
      (while "prepending scope")
      (guardScopeUpdate newScope)
      (modifyScope (scope: newScope // scope));

  prependScopeM = newScopeM: {_, ...}:
    _.do
      (while "prepending monadic scope")
      {newScope = newScopeM;}
      ({_, newScope}: _.prependScope newScope);

  appendScope = newScope: {_, ...}:
    _.do
      (while "appending scope")
      (guardScopeUpdate newScope)
      (modifyScope (scope: scope // newScope));

  appendScopeM = newScopeM: {_, ...}:
    _.do
      (while "appending monadic scope")
      {newScope = newScopeM;}
      ({_, newScope}: _.appendScope newScope);

  getWithScope = {_}: _.do
    (while "getting 'with' scope")
    {scope = getScope;}
    ({scope, _}: _.pure scope.__internal__.withScope);

  setWithScope = withScope: {_}: _.do
    (while "setting 'with' scope")
    {scope = getScope;}
    ({scope, _}: _.setScope (scope // {
      __internal__ = scope.__internal__ // { 
        inherit withScope; 
      };
    }));

  appendWithScope = withScope: {_}: _.do
    (while "appending 'with' scope")
    {scope = getScope;}
    ({scope, _}: _.setScope (scope // {
      __internal__ = scope.__internal__ // { 
        withScope = scope.__internal__.withScope // withScope;
      };
    }));

  traceWithScope = {_}: _.do
    {scope = getWithScope;}
    ({scope, _}: _.whileV 3 "tracing 'with' scope:\n${_p_ scope}");

  traceScope = {_}: _.do
    {scope = getScope;}
    ({scope, _}: _.whileV 3 "tracing scope:\n${_p_ scope}");

  CODE = thunkId: nodeType: {
    inherit nodeType;
    thunkId = toString thunkId;
    __isThunk = true;
    __toString = collective-lib.tests.expect.anyLambda;
    runWithCacheM = collective-lib.tests.expect.anyLambda;
  };

  expectEvalError = with tests; expectEvalErrorWith expect.noLambdasEq;

  expectEvalErrorWith = expectation: E: expr:
    let result = eval.ast.runAST' expr;
    in expectation (rec {
      resultIsLeft = isLeft result;
      resultEMatches = is E (result.left or null);
      inherit E;
      resultE = result.left or result.right;
    }) {
      resultIsLeft = true;
      resultEMatches = true;
      inherit E;
      resultE = result.left or null;
    };

  removeBuiltins = s:
    if EvalState.check s then EvalState { 
      scope = removeAttrs s.scope ["builtins"];
      inherit (s) thunkCache;
    } else if isAttrs s then removeAttrs s ["builtins"] else s;

  expectRun = {
    actual, 
    expected,
    initialScope ? initScope,
    includeBuiltins ? false,
    expectedScope ? null,
    buildExpectedScope ? mkInitScope,
    expectedThunkCache ? null
  }:
    with tests;
    let 
      maybeRemoveBuiltins = e: 
        if includeBuiltins then e else e // { 
          s = removeBuiltins e.s;
          a = removeBuiltins e.a;
        };

      initialState = mkInitEvalState initialScope;
      r = actual.run initialState;
    in 
      if isErrorType expected then
        expect.eqOn (e: e.left.__errorName or {__notLeft = e;})
          r
          {left = {__errorName = (E "").__errorName;};}

      else 
        expect.eqOn
          (e: 
            if e ? right then Compare.NoLambdas e.right
            else Compare.NoLambdas e)
          (r.fmap maybeRemoveBuiltins)
          {
            right = maybeRemoveBuiltins {
              s = EvalState {
                scope =
                  if expectedScope == null then r.right.s.scope 
                  else buildExpectedScope expectedScope;
                thunkCache =
                   def (r.right.s.thunkCache or null) expectedThunkCache;
              };
              a = expected;
            };
          };

  expectRunError = s: a: e: 
    with tests;
    expect.noLambdasEq
      ((a.run (mkInitEvalState s)).left or {__notLeft = true;})
      e;

  expectRunNixError = s: a: 
    with tests;
    expect.error
      (a.run (mkInitEvalState s));

  _tests = with tests;
    let
      Int = { 
        __toString = self: "Int";
        check = x: isInt (x.x or null);
        __functor = self: x: { 
          inherit x; 
          __type = Int; 
          __toString = self: "Int ${_p_ self.x}";
          }; 
      };
    in suite {
      _00_either =
        let
          E = Either EvalError Int;
        in with E; with EvalError; {
          left.isLeft = expect.True (isLeft (Left (Abort "test")));
          left.isRight = expect.False (isRight (Left (Abort "test")));
          left.wrongType = expect.error (Left (Int 1));
          right.isLeft = expect.False (isLeft (Right (Int 1)));
          right.isRight = expect.True (isRight (Right (Int 1)));
          right.wrongType = expect.error (Right (Abort "test"));
          left.fmap = expect.noLambdasEq ((Left (Abort "test")).fmap (x: Int (x.x + 1))) (Left (Abort "test"));
          right.fmap.sameType = expect.noLambdasEq ((Right (Int 1)).fmap (x: Int (x.x + 1))) (Right (Int 2));
          right.fmap.changeType = 
            let Right' = (Either EvalError parser.AST).Right;
            in expect.noLambdasEq ((Right (Int 1)).fmap (_: parser.N.int 42)) (Right' (parser.N.int 42));
        };

      _01_state = {
        mk.public = expect.noLambdasEq ((EvalState {scope = mkInitScope {};}).publicScope {}) {};
        mk.private = expect.noLambdasEq (EvalState {scope = mkInitScope {};}).scope (mkInitScope {});
        fmap =
          expect.noLambdasEq
          ((EvalState {scope = {};}).fmap (scope: scope // {x = 1;}))
          (EvalState {scope = {x = 1;};});
      };

      _02_monad = 
        let
          a = rec {
            _42 = Eval.pure (Int 42);
            stateXIs2 = _42.bind (set (mkInitEvalState { x = 2; }));
            stateXTimes3 = stateXIs2.bind (modify (s: mkInitEvalState { x = s.scope.x * 3; }));
            const42 = stateXTimes3.pure (Int 42);
            getStatePlusValue = 
              const42.bind ({_, _a}:
                let i = _a;
                in (_.bind _.get).bind ({_, _a}: _.pure (Int (_a.scope.x + i.x))));
            thenThrows = stateXTimes3.bind ({_}: _.throws (Throw "test error"));
            bindAfterThrow = thenThrows.bind ({_}: _.pure "not reached");
            
            catchAfterThrow = thenThrows.catch (e: {_}: _.pure "handled error '${e}'");
            fmapAfterCatch = catchAfterThrow.fmap (s: s + " then ...");
          };
        in with EvalState; {
          __smoke = {
            isMonadOf.monad = expect.True (isMonadOf Eval (Eval.pure unit));
            isMonadOf.do = expect.True (isMonadOf Eval (Eval.do (Eval.pure unit)));
            isMonadOf.false = expect.False (isMonadOf Eval ((Either EvalError Int).Right (Int 42)));

            getM.monad = expect.True (tEq Eval (getM (Eval.pure unit)));
            getM.do = expect.True (tEq Eval (getM (Eval.do (Eval.pure unit))));

            getT.monad = expect.True (tEq (Eval Unit) (getT (Eval.pure unit)));
            getT.do = expect.True (tEq (Eval Unit) (getT (Eval.do (Eval.pure unit))));
          };

          _00_chain = {
            _00_pure = expectRun { actual = a._42; expected = Int 42; };
            _01_set = expectRun { actual = a.stateXIs2; expected = unit; expectedScope = { x = 2; }; };
            _02_modify = expectRun { actual = a.stateXTimes3; expected = unit; expectedScope = { x = 6; }; };
            _04_bind.get = expectRun { actual = a.getStatePlusValue; expected = Int 48; expectedScope = { x = 6; }; };
            _05_bind.thenThrows = expectRunError {} a.thenThrows (Throw "test error");
            _06_bind.bindAfterThrow = expectRunError {} a.bindAfterThrow (Throw "test error");
            _07_catch.noError = expectRun { actual = (a._42.catch (_: throw "no")); expected = Int 42; };
            _08_catch.withError = expectRun { actual = a.catchAfterThrow; expected = "handled error 'EvalError.Throw:\n  test error'"; expectedScope = { x = 6; }; };
            _09_catch.thenFmap = expectRun { actual = a.fmapAfterCatch; expected = "handled error 'EvalError.Throw:\n  test error' then ..."; expectedScope = { x = 6; }; };
          };
          
          _01_signatures = {
            IndependentAction.monad =
              expect.eq (bindStatementSignature Eval (Eval.pure unit)) "IndependentAction";
            IndependentAction.do =
              expect.eq (bindStatementSignature Eval (Eval.do (Eval.pure unit))) "IndependentAction";
            IndependentBind =
              expect.eq (bindStatementSignature Eval {a = Eval.pure unit;}) "IndependentBind";
            DependentAction =
              expect.eq (bindStatementSignature Eval ({_}: _.pure unit)) "DependentAction";
            DependentBind =
              expect.eq (bindStatementSignature Eval {a = {_}: _.pure unit;}) "DependentBind";
          };

          _02_inferMonad = {
            IndependentAction.monad =
              expect.True (tEq Eval (inferMonadFromStatement (Eval.pure unit)));
            IndependentAction.do =
              expect.True (tEq Eval (inferMonadFromStatement (Eval.do (Eval.pure unit))));
            IndependentBind =
              expect.True (tEq Eval (inferMonadFromStatement {a = Eval.pure unit;}));
            DependentAction =
              expect.error (inferMonadFromStatement ({_}: _.pure unit));
            DependentBind =
              expect.error (inferMonadFromStatement {a = {_}: _.pure unit;});
          };

          _03_doNotation = {
            _00_basic = {
              const = expectRun { actual = (Eval.do (Eval.pure 123)); expected = 123; };

              constBound = expectRun { actual = (Eval.do ({_}: _.pure 123)); expected = 123; };

              bindOne =
                let m = Eval.do {x = Eval.pure 1;} ({_, ...}: _.pure unit);
                in expectRun { actual = m; expected = unit; };

              bindOneBound =
                let m = Eval.do {x = {_}: _.pure 1;} ({_}: _.pure unit);
                in expectRun { actual = m; expected = unit; };

              bindOneGetOne =
                let m = Eval.do {x = Eval.pure 1;} ({_, x}: _.pure x);
                in expectRun { actual = m; expected = 1; };

              dependentBindGet = 
                let m = Eval.do
                  {x = {_}: _.pure 1;}
                  {y = {_, x}: _.pure (x + 1);}
                  ({_, x, y}: _.pure (x + y));
                in expectRun { actual = m; expected = 3; };

              boundDo = 
                let do = Eval.do; in with Eval;
                let m = do
                  {x = Eval.pure 1;}
                  {y = Eval.pure 2;}
                  ({x, y, ...}: Eval.pure (x + y));
                in expectRun { actual = m; expected = 3; };

              guard.pass =
                let m = Eval.do
                  ( {_}: _.guard true (TypeError "fail"))
                  ( {_}: _.pure unit );
                in expectRun { actual = m; expected = unit; };

              guard.fail =
                let m = Eval.do
                  ( {_}: _.guard false (TypeError "fail"))
                  ( {_}: _.pure unit );
                in expectRunError {} m (TypeError "fail");
            };

            _01_setGet = {
              _00_helpers = {
                _00_get = expectRun {
                  actual = Eval.do (getPublicScope);
                  expected = initScope;
                };

                _01_set = expectRun { 
                  actual = Eval.do (setPublicScope {x = 1;});
                  expected = unit;
                  expectedScope = mkInitScope {x = 1;};
                };

                _02_setSet = expectRun { 
                  actual = Eval.do
                    (setPublicScope {x = 1;})
                    (setPublicScope {y = 2;});
                  expected = unit;
                  expectedScope = {y = 2;};
                };

                _03_setModGet = expectRun { 
                  actual = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: scope // {x = scope.x + 2; y = 2;}))
                    (getPublicScope);
                  expected = {x = 3; y = 2;};
                  expectedScope = {x = 3; y = 2;};
                };

                _04_setModGetBlocks = 
                  let sets = {_, ...}: _.setPublicScope {x = 1;};
                      mods = {_, ...}: _.modifyPublicScope (scope: scope // {x = scope.x + 2; y = 2;});
                      gets = {_, ...}: _.getPublicScope;
                      m = Eval.do sets mods gets;
                  in expectRun { actual = m; expected = {x = 3; y = 2;}; expectedScope = {x = 3; y = 2;}; };

                _05_setModGetBlocksDo = 
                  let sets = Eval.do ({_, ...}: _.setPublicScope {x = 1;});
                      mods = Eval.do ({_, ...}: _.modifyPublicScope (scope: scope // {x = scope.x + 2; y = 2;}));
                      gets = Eval.do ({_, ...}: _.getPublicScope);
                      m = Eval.do sets mods gets;
                  in expectRun { actual = m; expected = {x = 3; y = 2;}; expectedScope = {x = 3; y = 2;}; };

                _06_useScope = 
                  let 
                    getClear = Eval.do
                      {scope = getPublicScope;}
                      (setPublicScope {cleared = true;})
                      ({_, scope}: _.pure scope);

                    xInc4 = Eval.do
                      {scope = getPublicScope;}
                      ({_, scope}: _.modifyPublicScope (scope': scope' // {x = scope.x + 1;}))
                      (modifyPublicScope (scope: scope // {x = scope.x + 3;}));

                    m = Eval.do
                      ({_}: _.setPublicScope {x = 1;})
                      xInc4
                      xInc4
                      getClear;

                  in expectRun { actual = m; expected = {x = 9;}; expectedScope = {cleared = true;}; };
              };

              _01_blocks = {
                _00_do =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                _01_chainBlocks =
                  let a = Eval.do (setPublicScope {x = 1;});
                      b = Eval.do a getPublicScope;
                      m = Eval.do b;
                  in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                _02_withoutDo =
                  let a = setPublicScope {x = 1;};
                      b = modifyPublicScope (scope: scope // {x = scope.x + 1;});
                      c = {_}: (_.bind _.getPublicScope).bind ({_, _a}: _.pure _a.x);
                      m = (((Eval.pure unit).bind a).bind b).bind c;
                  in expectRun { actual = m; expected = 2; expectedScope = {x = 2;}; };

                _03_getScope =
                  expectRun {
                    actual = Eval.do
                      (setPublicScope {x = 1;})
                      getPublicScope;
                    expected = {x = 1;};
                    expectedScope = {x = 1;};
                  };

                _04_appendScope =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: scope // {y = 2;}))
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                _05_overwriteScopeAppend =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: scope // {x = 2;}))
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 2;}; expectedScope = {x = 2;}; };

                _06_overwriteScopePrepend =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (modifyPublicScope (scope: {x = 2;} // scope))
                    getPublicScope;
                  in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };
                
                _07_saveScopeAppendScope =
                  let m = Eval.do
                    (setPublicScope {x = 1;})
                    (saveScope ({_}: _.do
                      (appendScope {y = 2;})
                      getPublicScope
                    ));
                  in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1;}; };

                _08_nestedSaveScope =
                  expectRun { 
                    actual = 
                      Eval.do
                        (setPublicScope {x = 1;})
                        (saveScope ({_}: _.do
                          (appendScope {y = 2;})
                          (saveScope ({_}: _.do
                            (appendScope {z = 3;})
                            getPublicScope
                          ))
                        ));
                    expected = {x = 1; y = 2; z = 3;};
                    expectedScope = {x = 1;};
                  };

                _09_withScope = {
                  _00_empty = 
                    expectRun { 
                      actual = (Eval.do (setWithScope {})); 
                      expected = unit; 
                    };
                  _01_set = 
                    expectRun {
                      actual = (Eval.do (setWithScope {x = 1;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope.x = 1; };
                    };
                  _02_append = 
                    expectRun {
                      actual = (Eval.do (appendWithScope {x = 1;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope.x = 1; };
                    };
                  _03_overwrite = 
                    expectRun {
                      actual = (Eval.do (setWithScope {x = 2;}) (appendWithScope {x = 3;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope.x = 3; };
                    };
                  _04_appendAppend = 
                    expectRun { 
                      actual = (Eval.do (appendWithScope {x = 1;}) (appendWithScope {y = 2;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope = {x = 1; y = 2;}; };
                    };
                  _05_appendAppendAppend = 
                    expectRun { 
                      actual = (Eval.do (appendWithScope {x = 1;}) (appendWithScope {y = 2;}) (appendWithScope {z = 3;})); 
                      expected = unit;
                      buildExpectedScope = id;
                      expectedScope = testScope { __internal__.withScope = {x = 1; y = 2; z = 3;}; };
                    };
                };

                _10_differentBlocks = {
                  _00_differentBlocks =
                    let a = setPublicScope {x = 1;};
                        b = getPublicScope;
                        m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _01_differentBlocksBind =
                    let a = setPublicScope {x = 1;};
                        b = getPublicScope;
                        m = ((Eval.pure unit).bind a).bind b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _02_differentDoBlocks =
                    let a = {_}: _.do (setPublicScope {x = 1;});
                        b = {_}: _.do getPublicScope;
                        m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _03_differentEvalDoBlocks =
                    let a = Eval.do (setPublicScope {x = 1;});
                        b = Eval.do getPublicScope;
                        m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1;}; expectedScope = {x = 1;}; };

                  _04_appendScopeDifferentEvalBlock =
                    let 
                      a = 
                        Eval.do 
                          (setPublicScope {x = 1;})
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      m = Eval.do a;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _05_appendScopeDifferentEvalBlocks =
                    let 
                      a = 
                        Eval.do 
                          (setPublicScope {x = 1;})
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      b = 
                        Eval.do 
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      m = Eval.do a b;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _06_appendScopeDifferentBlock =
                    let 
                      a = 
                        {_}: _.do 
                          (setPublicScope {x = 1;})
                          (modifyPublicScope (scope: scope // {y = 2;}))
                          getPublicScope;
                      m = Eval.do a;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _07_appendScopeDifferentBlocks =
                    let 
                      a = {_}: _.do (setPublicScope {x = 1;});
                      b = {_}: _.do (modifyPublicScope (scope: scope // {y = 2;}));
                      c = {_}: _.do getPublicScope;
                      m = Eval.do a b c;
                    in expectRun { actual = m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };

                  _08_appendScopeDifferentBlocksBindNoDo =
                    let 
                      a = setPublicScope {x = 1;};
                      b = modifyPublicScope (scope: scope // {y = 2;});
                      c = getPublicScope;
                      m = (((Eval.pure unit).bind a).bind b).bind c;
                    in expectRun { actual = Eval.do m; expected = {x = 1; y = 2;}; expectedScope = {x = 1; y = 2;}; };
                };
              };

              _02_printDo = {
                minimalDoBlock = expect.eq (toString testData.minimalDoBlock) (_b_ ''
                  Eval.do <monad.nix:14>
                    {a = Eval.pure "pure string";}
                    ({_, a, ...}: (<Eval Unit>))
                '');
                doBlock = expect.eq (toString testData.doBlock) (_b_ ''
                  Eval.do <monad.nix:19>
                    {a = Eval.pure "independent bind";}
                    {b = pure "implicit dependent bind";}
                    {c = {_}: _.pure "implicit dependent bind";}
                    (<Eval string>)
                    ({_, ...}: (<Eval Unit>))
                    ({_, ...}: (<Eval Unit>))
                    (Eval.do <monad.nix:26>
                      {a = Eval.pure "nested independent bind";}
                      {b = pure "nested implicit dependent bind";}
                      {c = {_}: _.pure "nested implicit dependent bind";}
                      (<Eval string>)
                      ({_, ...}: (<Eval Unit>))
                      ({_, ...}: (<Eval Unit>)))
                '');
              };
              
              _03_composes = 
                let a = Eval.do (modifyPublicScope (scope: scope // {x = 1;}));
                    b = Eval.do (modifyPublicScope (scope: scope // {y = 2;}));
                in {
                  _00_scopeExists = expectRun { 
                    actual = a; 
                    expected = unit;
                    buildExpectedScope = testScope;
                    expectedScope = {x = 1;};
                  };

                  #_01_doBind = expectRun { 
                  #  actual = Eval.do a b;
                  #  expected = unit;
                  #  buildExpectedScope = testScope;
                  #  expectedScope = {x = 1; y = 2;};
                  #};

                  #_02_doSq = expectRun { 
                  #  actual = a.sq b;
                  #  expected = unit;
                  #  buildExpectedScope = testScope;
                  #  expectedScope = {x = 1; y = 2;};
                  #};
                };
            };

            _02_functions = {
              _00_foldM = {
                empty = expectRun { actual = (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [])); expected = 0; };
                single = expectRun { actual = (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [5])); expected = 5; };
                multiple = expectRun { actual = (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [1 2 3])); expected = 6; };
              };

              _01_traverse = {
                _00_empty = expectRun { actual = (Eval.do ({_}: _.traverse (x: Eval.pure (x + 1)) [])); expected = []; };
                _01_single = expectRun { actual = (Eval.do ({_}: _.traverse (x: Eval.pure (x + 1)) [5])); expected = [6]; };
                _02_multiple = expectRun { actual = (Eval.do ({_}: _.traverse (x: Eval.pure (x * 2)) [1 2 3])); expected = [2 4 6]; };
                
                # Simple test showing get() works correctly in do-block  
                _03_simpleGetIssue = expectRun {
                  actual = (
                    Eval.do
                      (set (mkInitEvalState {test = "value";}))
                      {stateAfterSet = get;}
                      ({_, stateAfterSet}: _.pure (stateAfterSet.publicScope {}))
                  );
                  expected = { test = "value"; };
                  expectedScope = { test = "value"; };
                };
                
                # Test that traverse properly threads state with foldM implementation
                _04_traverseWithState = expectRun {
                  actual = (
                    Eval.do
                      (set (mkInitEvalState {counter = 0; seen = [];}))
                      {result = {_, ...}: _.traverse (x: 
                        Eval.do
                          {state = get;}
                          ({_, state}: _.set (EvalState {scope = state.scope // {
                            counter = (state.scope.counter or 0) + x;
                            seen = (state.scope.seen or []) ++ [x];
                          };}))
                          ({_}: _.pure x)
                      ) [1 2 3 4 5];}
                      {finalState = get;}
                      ({_, result, finalState}: _.pure {
                        result = result;
                        finalCounter = finalState.scope.counter;
                      })
                  );
                  expected = {
                    result = [1 2 3 4 5];
                    finalCounter = 15;
                  };
                  expectedScope = { counter = 15; seen = [1 2 3 4 5]; };
                };
              };
            };

            _03_thunkCache = with parser; {
              _00_getEmpty =
                expectRun {
                  actual =
                    Eval.do
                      getThunkCache;
                  expected = ThunkCache {};
                };

              _01_put = 
                expectRun {
                  actual =
                    Eval.do
                      (Thunk (N.string "x"));
                  expected = CODE 0 "string";
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; };
                    nextId = 1;
                  };
                };

              _02_putGet = 
                expectRun {
                  actual =
                    Eval.do
                      {x = Thunk (N.string "x");}
                      ({x, _}: force x);
                  expected = "x";
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; };
                    values = { "0" = "x"; };
                    misses = 1;
                    hits = 0;
                    nextId = 1;
                  };
                };

              _03_putGetGet = 
                expectRun {
                  actual =
                    Eval.do
                      {x = Thunk (N.string "x");}
                      ({x, _}: _.do
                        {x'0 = force x;} # Miss
                        {x'1 = force x;} # Hit
                        ({x'0, x'1, _}: _.pure [x'0 x'1]));
                  expected = ["x" "x"];
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; };
                    values = { "0" = "x"; };
                    misses = 1;
                    hits = 1;
                    nextId = 1;
                  };
                };

              _04_putGetMany = 
                expectRun {
                  actual =
                    Eval.do
                    {x = Thunk (N.string "x");}
                    {y = Thunk (N.int 1);}
                    ({x, y, _}: _.do
                      {x'0 = force x;} # Miss
                      {x'1 = force x;} # Hit
                      {x'2 = force x;} # Hit
                      {y'0 = force y;} # Miss
                      {x'3 = force x;} # Hit
                      {y'1 = force y;} # Hit
                      {x'4 = force x;} # Hit
                      ({x'0, x'1, x'2, x'3, x'4, y'0, y'1, _}: _.pure [x'0 x'1 x'2 x'3 x'4 y'0 y'1]));
                  expected = ["x" "x" "x" "x" "x" 1 1];
                  expectedThunkCache = ThunkCache {
                    thunks = { "0" = CODE 0 "string"; "1" = CODE 1 "int"; };
                    values = { "0" = "x"; "1" = 1; };
                    misses = 2;
                    hits = 5;
                    nextId = 2;
                  };
                };

              #_04_thunksCaptureScope = 
              #  expectRun {
              #    actual = 
              #      Eval.do
              #        (setScope {a = 1;})
              #        {x = Thunk (N.list (N.identifier "a"));}
              #        (setScope {a = 2;})
              #        {y = Thunk (N.list (N.identifier "a"));}
              #        ({x, y, _}: _.pure (x ++ y));
              #    expected = [1 2];
              #    expectedScope = {a = 2;};
              #    expectedThunkCache = ThunkCache {
              #      thunks = {
              #        "0" = CODE 0 "list";
              #        "1" = CODE 1 "identifier";
              #        "2" = CODE 2 "list";
              #        "3" = CODE 3 "identifier";
              #      };
              #      values = { 
              #        "0" = [ (Code 1 "identifier") ];
              #        "1" = 1;
              #        "2" = [ (Code 3 "identifier") ];
              #        "3" = 2;
              #      };
              #      misses = 0;
              #      hits = 4;
              #      nextId = 5;
              #  };
              #};
          };

          _04_syntax = {
            _00_missingBind =
              expectRun {
                actual = (Eval.do ({a, _}: _.pure a));
                expected = SyntaxError;
              };
            _01_missingBindNotCatchable =
              expectRun {
                actual = ((Eval.do ({a, _}: _.pure a)).catch (_e: pure "got ${_e}"));
                expected = SyntaxError;
              };
          };

          _05_runM = {
            _00_simple = expectRun {
              actual = (Eval.do
                (runM (Eval.do (pure 1))));
              expected = 1;
            };

            _01_bindingsDontLeakIntoRunM =
              expectRun {
                actual = (Eval.do
                  {a = pure 1;}
                  (runM (Eval.do ({a, _}: _.pure a))));
                expected = SyntaxError;
              };

            _02_scopeDoesntLeakIntoRunM =
              expectRun {
                actual = 
                  Eval.do
                    (setPublicScope {a = 1;})
                    (runM (Eval.do 
                      {scope = {_}: _.getScope;}
                      ({scope, _}: _.pure (scope.a or null))));
                expected = null;
                expectedScope = {a = 1;};
              };
          };

        };
      };
    };
}
