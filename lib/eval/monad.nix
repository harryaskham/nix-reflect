{ lib, collective-lib, nix-reflect, ... }:

with collective-lib.typed;
let
  inherit (nix-reflect) parser debuglib;
in rec {
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
      check = x: (x ? __isEvalError) && (x ? "__isEvalError${name}");
      __functor = self: __msg: {
        __type = EvalError;
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
  Abort = EvalError "Abort";
  AssertError = EvalError "AssertError";
  Throw = EvalError "Throw";
  TypeError = EvalError "TypeError";
  RuntimeError = EvalError "RuntimeError";
  UnknownIdentifierError = EvalError "UnknownIdentifierError";
  MissingAttributeError = EvalError "MissingAttributeError";
  NixPathError = EvalError "NixPathError";

  isEvalError = x: x ? __isEvalError;

  EvalState = rec {
    __toString = self: "EvalState";
    check = x: x ? __isEvalState;
    mempty = _: EvalState {};
    mconcat = ss: EvalState (mergeAttrsList (map (s: s.scope) ss));
    __functor = self: scope: {
      __type = EvalState;
      __isEvalState = true;
      __toString = self: _b_ "EvalState ${_ph_ self.scope}";
      inherit scope;
      fmap = f: EvalState (f scope);
    };
  };

  # TODO: cwd, other impure state
  initEvalState = EvalState initScope;
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
    builtins = (removeAttrs builtins ["builtins"]) // (with parser; {
      "throw" = N.throwExpr;
      "abort" = N.abortExpr;
      "assert" = N.assertExpr;
      # TODO: Native import
      #"import" = ...
    });
    # Expose same builtins on top-level as Nix
    inherit (self.builtins) "derivation" "import" "throw" "abort";

    # Expose minimal lib to avoid error blowup
    lib = { inherit (lib) isFunction attrValues; };
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
    (M != null && isMonadOf M xs)
    || (isFunction xs)
    || (isSolo xs && isDoStatement M (soloValue xs));

  assertIsDoStatement = M: xs:
    assert that (isDoStatement M xs) ''
      do: expected a statement of form

        ( ${if M == null then "<monadic value>" else M Any} )

      or

        ( { bindings, ... }: ${if M == null then "<monadic value>" else M Any} )

      or

        { name = ${if M == null then "<monadic value>" else M Any}; }

      or

        { name = ({ bindings, ... }: ${if M == null then "<monadic value>" else M Any}); }

      but got ${_p_ xs}
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

  printDo = self:
    let 
      doPos = 
        if empty self.__statements then null
        else debuglib.pos.path (head self.__statements);

      doLine = 
        if doPos == null then null
        else let p = soloValue doPos; in 
            if (p ? file) && (p ? line) then "${p.file}:${toString p.line}"
            else null;
      header = optionalString (doLine != null) doLine;
    in 
      if empty self.__statements then "<empty do-block>"
      else _b_ ''
        ${header}
        do
          ${_h_ (_ls_ (map (debuglib.printPosWith {
            emptyMsg = "<no source>";
            errorMsg = "<error>";
          }) self.__statements))}
      '';

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
  handleBindStatement = M: acc: statement:
    assert (assertIsDoStatement M statement);
    let normalised = normaliseBindStatement M statement;
    in
      (acc.m.bind ({_, _a}:
        let mb_ = normalised.f (acc.bindings // { inherit _ _a; });
            mb = if isDo mb_ then mb_.__setInitM acc.m else mb_;
        in
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
    let this = {
      __isDo = true;
      __isMonad = true;
      __type = inferTypeFromStatement (maybeLast __statements);
      inherit M __initM __statements;

      __toString = printDo;

      __functor = self: statement:
        mkDo M self.__initM (self.__statements ++ [statement]);

      __setInitM = initM: mkDo this.M initM this.__statements;

      # Bind with specified initial monadic value.
      # Just creates a new do with self as the initial value of the last one.
      bind = statement:
        M.do
          (this.action.bind statement);

      sq = b: this.bind (_: b);

      # Actually force the block to evaluate down to a final M value.
      force = {}:
        let 
          initAcc = {
            bindings = {};
            canBind = false;
            m = this.__initM;
          };
          accM = (M.pure unit).foldM (handleBindStatement M) initAcc (this.__statements);
        in
          accM.bind ({_, _a}:
            assert that _a.canBind (withStackError this ''
              do: final statement of a do-block cannot be an assignment.
            '');
            _a.m);

      action = this.force {};
      run = arg: this.action.run arg;
      run_ = arg: this.action.run_ arg;
      runEmpty = arg: this.action.runEmpty arg;
      runEmpty_ = arg: this.action.runEmpty_ arg;
      runClosure = arg: this.action.runClosure arg;
      runClosureM = arg: this.action.runClosureM arg;
      mapState = arg: this.action.mapState arg;
      setState = arg: this.action.setState arg;
      mapEither = arg: this.action.mapEither arg;
      catch = arg: this.action.catch arg;
      do = mkDo M this.action [];
    };
    in this;

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

              # Thunked to avoid infinite nesting - (m.get {}) is an (Eval EvalState)
              get = this: {_, ...}: _.pure (this.s (S.mempty {}));

              setState = this: s: set_s this s;
              mapState = this: f: this.setState (f this.s);

              setEither = this: e: set_e this e;
              mapEither = this: f: this.setEither (f this.e);
              liftEither = this: e: if is EvalError e then this.throws e else this.pure e;

              getScope = this: this.bind getScope;
              setScope = this: newScope: this.bind (setScope newScope);
              saveScope = this: f: this.bind (saveScope f);
              modifyScope = this: f: this.bind (modifyScope f);
              prependScope = this: newScope: this.bind (prependScope newScope);
              appendScope = this: newScope: this.bind (appendScope newScope);

              do = this: statement: mkDo Eval this [] statement;
              pure = this: x: set_e_Right this x;
              fmap = this: f: set_e this (this.e.fmap f);
              when = this: cond: m: if cond then this.bind m else this.pure unit;
              unless = this: cond: m: if !cond then this.bind m else this.pure unit;
              whileV = this: v: s:
                # Add the stack logging to the monadic value itself
                log.while s (
                  # Add runtime tracing to the resolution of the bind only
                  this.bind (_: (log.v v).showN "while ${s}" this)
                );
              while = this: s: this.whileV 3 s;
              guard = this: cond: e: 
                if cond 
                then this.bind ({_}: _.pure unit) 
                else (this.throws e);

              foldM = this: f: initAcc: xs:
                let startM = this.pure initAcc;
                in fold.left (accM: a: accM.bind ({_, _a}: _.bind (f _a a))) startM xs;

              # sequenceM :: [Eval a] -> Eval [a]
              sequenceM = this:
                this.foldM
                  (acc: elemM:
                    Eval.do
                      {elem = elemM;}
                      ({_, elem}: _.pure (acc ++ [elem])))
                  [];

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
                        mb = normalised.f {_ = this; _a = a;};
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
                if isLeft this.e then 
                  (set_e_Right this unit)
                  .bind ({_, ...}: handler {inherit _; _e = this.e.left;})
                else this;

              # Returns (Either EvalError { a :: A, s :: S })
              run = this: initialState: 
                this.e.fmap (a: { s = this.s initialState; inherit a; });
              run_ = this: _: this.e;
              runEmpty = this: _: (this.run (S.mempty {}));
              runEmpty_ = this: _: ((this.run (S.mempty {})).fmap (r: r.a));
              runClosure = this: _: (this.runEmpty_ {}).unwrap;
              runClosureM = this: {_, ...}: _.pure (this.runClosure {});
            };
          };
        # Bind 'this'
        in rebind this {};
    };
  };

  sequenceM = this: xs: {_, ...}: _.sequenceM xs;
  foldM = f: initAcc: xs: {_, ...}: _.foldM f initAcc xs;
  traverse = f: xs: {_, ...}: _.traverse f xs;

  get = {_, ...}: _.bind _.get;
  set = state: {_, ...}: _.bind (_.set state);
  modify = f: {_, ...}: _.bind (_.modify f);
  liftEither = e: {_, ...}: _.liftEither e;

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
      (set (EvalState scope));
  
  modifyScope = f: {_, ...}:
    _.do
      (while "modifying scope")
      (modify (s: s.fmap f));

  getScope = {_, ...}:
    _.do
      (while "getting scope")
      {state = get;}
      ({_, state}: _.pure state.scope);

  prependScope = newScope: {_, ...}:
    _.do
      (while "prepending scope")
      (modifyScope (scope: newScope // scope));

  prependScopeM = newScopeM: {_, ...}:
    _.do
      (while "prepending monadic scope")
      {newScope = newScopeM;}
      ({_, newScope}: _.prependScope newScope);

  appendScope = newScope: {_, ...}:
    _.do
      (while "appending scope")
      (modifyScope (scope: scope // newScope));

  appendScopeM = newScopeM: {_, ...}:
    _.do
      (while "appending monadic scope")
      {newScope = newScopeM;}
      ({_, newScope}: _.appendScope newScope);

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
      either =
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

      state = {
        mk = expect.eq (EvalState {}).scope {};
        fmap =
          expect.noLambdasEq
          ((EvalState {}).fmap (scope: scope // {x = 1;}))
          (EvalState {x = 1;});
      };

      monad = 
        let
          a = rec {
            _42 = Eval.pure (Int 42);
            stateXIs2 = _42.bind (set (EvalState { x = 2; }));
            stateXTimes3 = stateXIs2.bind (modify (s: EvalState { x = s.scope.x * 3; }));
            const42 = stateXTimes3.pure (Int 42);
            getStatePlusValue = 
              const42.bind ({_, _a}:
                let i = _a;
                in (_.bind _.get).bind ({_, _a}: _.pure (Int (_a.scope.x + i.x))));
            thenThrows = stateXTimes3.bind ({_}: _.throws (Throw "test error"));
            bindAfterThrow = thenThrows.bind ({_}: _.pure "not reached");
            
            catchAfterThrow = thenThrows.catch ({_, _e}: _.pure "handled error '${_e}'");
            fmapAfterCatch = catchAfterThrow.fmap (s: s + " then ...");
          };
          expectRun = s: a: s': a': 
            expect.noLambdasEq
              (a.run (EvalState s)).right
              { s = EvalState s'; a = a'; };
          expectRunError = s: a: e: 
            expect.noLambdasEq
              (a.run (EvalState s)).left
              e;
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

          _00_pure = expectRun {} a._42 {} (Int 42);
          _01_set = expectRun {} a.stateXIs2 { x = 2; } unit;
          _02_modify = expectRun {} a.stateXTimes3 { x = 6; } unit;
          _04_bind.get = expectRun {} a.getStatePlusValue { x = 6; } (Int 48);
          _05_bind.thenThrows = expectRunError {} a.thenThrows (Throw "test error");
          _06_bind.bindAfterThrow = expectRunError {} a.bindAfterThrow (Throw "test error");
          _07_catch.noError = expectRun {} (a._42.catch (_: throw "no")) {} (Int 42);
          _08_catch.withError = expectRun {} a.catchAfterThrow { x = 6; } "handled error 'EvalError.Throw:\n  test error'";
          _09_catch.thenFmap = expectRun {} a.fmapAfterCatch { x = 6; } "handled error 'EvalError.Throw:\n  test error' then ...";
          

          _10_signatures = {
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

          _11_inferMonad = {
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

          do.notation = {
            const = expectRun {} (Eval.do (Eval.pure 123)) {} 123;

            constBound = expectRun {} (Eval.do ({_}: _.pure 123)) {} 123;

            bindOne =
              let m = Eval.do {x = Eval.pure 1;} ({_, ...}: _.pure unit);
              in expectRun {} m {} unit;

            bindOneBound =
              let m = Eval.do {x = {_}: _.pure 1;} ({_}: _.pure unit);
              in expectRun {} m {} unit;

            bindOneGetOne =
              let m = Eval.do {x = Eval.pure 1;} ({_, x}: _.pure x);
              in expectRun {} m {} 1;

            dependentBindGet = 
              let m = Eval.do
                {x = {_}: _.pure 1;}
                {y = {_, x}: _.pure (x + 1);}
                ({_, x, y}: _.pure (x + y));
              in expectRun {} m {} 3;

            boundDo = 
              let do = Eval.do; in with Eval;
              let m = do
                {x = Eval.pure 1;}
                {y = Eval.pure 2;}
                ({x, y, ...}: Eval.pure (x + y));
              in expectRun {} m {} 3;

            guard.pass =
              let m = Eval.do
                ( {_}: _.guard true (TypeError "fail"))
                ( {_}: _.pure unit );
              in expectRun {} m {} unit;

            guard.fail =
              let m = Eval.do
                ( {_}: _.guard false (TypeError "fail"))
                ( {_}: _.pure unit );
              in expectRunError {} m (TypeError "fail");

            setGet = {
              helpers = {
                get = 
                  let m = Eval.do
                    (getScope);
                  in expectRun {} m {} {};

                set = 
                  let m = Eval.do (setScope {x = 1;});
                  in expectRun {} m {x = 1;} unit;

                setSet = 
                  let m = Eval.do (setScope {x = 1;}) (setScope {y = 2;});
                  in expectRun {} m {y = 2;} unit;

                setModGet = 
                  let m = Eval.do
                    (setScope {x = 1;})
                    (modifyScope (scope: scope // {x = scope.x + 2; y = 2;}))
                    (getScope);
                  in expectRun {} m {x = 3; y = 2;} {x = 3; y = 2;};

                setModGetBlocks = 
                  let sets = {_, ...}: _.setScope {x = 1;};
                      mods = {_, ...}: _.modifyScope (scope: scope // {x = scope.x + 2; y = 2;});
                      gets = {_, ...}: _.getScope;
                      m = Eval.do sets mods gets;
                  in expectRun {} m {x = 3; y = 2;} {x = 3; y = 2;};

                setModGetBlocksDo = 
                  let sets = Eval.do ({_, ...}: _.setScope {x = 1;});
                      mods = Eval.do ({_, ...}: _.modifyScope (scope: scope // {x = scope.x + 2; y = 2;}));
                      gets = Eval.do ({_, ...}: _.getScope);
                      m = Eval.do sets mods gets;
                  in expectRun {} m {x = 3; y = 2;} {x = 3; y = 2;};

                useScope = 
                  let 
                    getClear = Eval.do
                      {scope = getScope;}
                      (setScope {cleared = true;})
                      ({_, scope}: _.pure scope);

                    xInc4 = Eval.do
                      {scope = getScope;}
                      ({_, scope}: _.modify (s: s.fmap (scope': scope' // {x = scope.x + 1;})))
                      (modifyScope (scope: scope // {x = scope.x + 3;}));

                    m = Eval.do
                      ({_}: _.setScope {x = 1;})
                      xInc4
                      xInc4
                      getClear;

                  in expectRun {} m {cleared = true;} {x = 9;};
              };

              do =
                let m = Eval.do
                  (set (EvalState {x = 1;}))
                  get;
                in expectRun {} m {x = 1;} (EvalState {x = 1;});

              chainBlocks =
                let a = Eval.do (set (EvalState {x = 1;}));
                    b = Eval.do a get;
                    m = Eval.do b;
                in expectRun {} m {x = 1;} (EvalState {x = 1;});

              withoutDo =
                let a = set (EvalState {x = 1;});
                    b = modify (s: s.fmap (scope: {x = scope.x + 1;}));
                    c = {_}: (_.bind _.get).bind ({_, _a}: _.pure _a.scope.x);
                    m = (((Eval.pure unit).bind a).bind b).bind c;
                in expectRun {} m {x = 2;} 2;

              getScope =
                let m = Eval.do
                  (set (EvalState ({x = 1;})))
                  {state = get;}
                  ({_, state}: _.pure state.scope);
                in expectRun {} m {x = 1;} {x = 1;};

              appendScope =
                let m = Eval.do
                  (set (EvalState ({x = 1;})))
                  (modify (s: s.fmap (scope: scope // {y = 2;})))
                  {state = get;}
                  ({_, state}: _.pure state.scope);
                in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};

              overwriteScopeAppend =
                let m = Eval.do
                  (set (EvalState ({x = 1;})))
                  (modify (s: s.fmap (scope: scope // {x = 2;})))
                  {state = get;}
                  ({_, state}: _.pure state.scope);
                in expectRun {} m {x = 2;} {x = 2;};

              overwriteScopePrepend =
                let m = Eval.do
                  (set (EvalState ({x = 1;})))
                  (modify (s: s.fmap (scope: {x = 2;} // scope)))
                  {state = get;}
                  ({_, state}: _.pure state.scope);
                in expectRun {} m {x = 1;} {x = 1;};
              
              saveScopeAppendScope =
                let m = Eval.do
                  (setScope {x = 1;})
                  (saveScope ({_}: _.do
                    (appendScope {y = 2;})
                    getScope
                  ));
                in expectRun {} m {x = 1;} {x = 1; y = 2;};

              nestedSaveScope =
                let m = Eval.do
                  (setScope {x = 1;})
                  (saveScope ({_}: _.do
                    (appendScope {y = 2;})
                    (saveScope ({_}: _.do
                      (appendScope {z = 3;})
                      getScope
                    ))
                  ));
                in expectRun {} m {x = 1;} {x = 1; y = 2; z = 3;};

              differentBlocks = {

                differentBlocks =
                  let a = set (EvalState {x = 1;});
                      b = get;
                      m = Eval.do a b;
                  in expectRun {} m {x = 1;} (EvalState {x = 1;});

                differentBlocksBind =
                  let a = set (EvalState {x = 1;});
                      b = get;
                      m = ((Eval.pure unit).bind a).bind b;
                  in expectRun {} m {x = 1;} (EvalState {x = 1;});

                differentDoBlocks =
                  let a = {_}: _.do (set (EvalState {x = 1;}));
                      b = {_}: _.do get;
                      m = Eval.do a b;
                  in expectRun {} m {x = 1;} (EvalState {x = 1;});

                differentEvalDoBlocks =
                  let a = Eval.do (set (EvalState {x = 1;}));
                      b = Eval.do get;
                      m = Eval.do a b;
                  in expectRun {} m {x = 1;} (EvalState {x = 1;});

                appendScopeDifferentEvalBlock =
                  let 
                    a = 
                      Eval.do 
                        (set (EvalState ({x = 1;})))
                        (modify (s: s.fmap (scope: scope // {y = 2;})))
                        {state = get;}
                        ({_, state}: _.pure state.scope);
                    m = Eval.do a;
                  in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};

                appendScopeDifferentEvalBlocks =
                  let 
                    a = 
                      Eval.do 
                        (set (EvalState ({x = 1;})))
                        (modify (s: s.fmap (scope: scope // {y = 2;})))
                        {state = get;}
                        ({_, state}: _.pure state.scope);
                    b = 
                      Eval.do 
                        (modify (s: s.fmap (scope: scope // {y = 2;})))
                        {state = get;}
                        ({_, state}: _.pure state.scope);
                    m = Eval.do a b;
                  in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};

                appendScopeDifferentBlock =
                  let 
                    a = 
                      {_}: _.do 
                        (set (EvalState ({x = 1;})))
                        (modify (s: s.fmap (scope: scope // {y = 2;})))
                        {state = get; }
                        ({_, state}: _.pure state.scope);
                    m = Eval.do a;
                  in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};

                appendScopeDifferentBlocks =
                  let 
                    a = {_}: _.do (set (EvalState ({x = 1;})));
                    b = {_}: _.do (modify (s: s.fmap (scope: scope // {y = 2;})));
                    c = {_}: _.do 
                      {state = get;}
                      ({_, state}: _.pure state.scope);
                    m = Eval.do a b c;
                  in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};

                appendScopeDifferentBlocksBindNoDo =
                  let 
                    a = modify (s: s.fmap (const {x = 1;}));
                    b = modify (s: s.fmap (scope: scope // {y = 2;}));
                    c = {_}: (_.bind _.get).bind ({_, _a, ...}: _.pure _a.scope);
                    m = (((Eval.pure unit).bind a).bind b).bind c;
                  in expectRun {} m {x = 1; y = 2;} {x = 1; y = 2;};
              };

            };
              
            composes = 
              let a = Eval.do (modify (s: s.fmap (scope: scope // {x = 1;})));
                  b = Eval.do (modify (s: s.fmap (scope: scope // {y = 2;})));
              in {
                scopeExists = expectRun {} a {x = 1;} unit;

                #do = 
                #  let c = 
                #    Eval.do 
                #      a
                #      b
                #      ( {_}: _.get {} );
                #  in expectRun {} c {x = 1; y = 2;} (EvalState {x = 1; y = 2;});

                doBind =
                  let c = (a.bind ({_}: b.bind ({_}: _.pure unit))).bind ({_}: _.bind _.get);
                  in expectRun {} c {x = 1; y = 2;} (EvalState {x = 1; y = 2;});

                doSq =
                  let c = (a.sq b).bind ({_}: _.bind _.get);
                  in expectRun {} c {x = 1; y = 2;} (EvalState {x = 1; y = 2;});
              };

              foldM = {
                empty = expectRun {} (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [])) {} 0;
                single = expectRun {} (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [5])) {} 5;
                multiple = expectRun {} (Eval.do ({_}: _.foldM (acc: x: Eval.pure (acc + x)) 0 [1 2 3])) {} 6;
              };

              traverse = {
                empty = expectRun {} (Eval.do ({_}: _.traverse (x: Eval.pure (x + 1)) [])) {} [];
                single = expectRun {} (Eval.do ({_}: _.traverse (x: Eval.pure (x + 1)) [5])) {} [6];
                multiple = expectRun {} (Eval.do ({_}: _.traverse (x: Eval.pure (x * 2)) [1 2 3])) {} [2 4 6];
                
                # Simple test showing get() works correctly in do-block  
                simpleGetIssue = expectRun {} (
                  Eval.do
                    (set (EvalState {test = "value";}))
                    {stateAfterSet = get;}
                    ({_, stateAfterSet}: _.pure stateAfterSet.scope)
                ) { test = "value"; } { test = "value"; };
                
                # Test that traverse properly threads state with foldM implementation
                traverseWithState = expectRun {}
                  (Eval.do
                    (set (EvalState {counter = 0; seen = [];}))
                    {result = {_, ...}: _.traverse (x: 
                      Eval.do
                        {state = get;}
                        ({_, state}: _.set (EvalState (state.scope // {
                          counter = (state.scope.counter or 0) + x;
                          seen = (state.scope.seen or []) ++ [x];
                        })))
                        ({_}: _.pure x)
                    ) [1 2 3 4 5];}
                    {finalState = get;}
                    ({_, result, finalState}: _.pure {
                      result = result;
                      finalCounter = finalState.scope.counter;
                    }))
                  { counter = 15; seen = [1 2 3 4 5]; } {
                  result = [1 2 3 4 5];
                  finalCounter = 15;
                };
              };

              sequenceM = {
                empty = expectRun {} ((Eval.pure unit).sequenceM []) {} [];
                single = expectRun {} ((Eval.pure unit).sequenceM [({_, ...}: _.pure 42)]) {} [42];
                multiple = expectRun {} ((Eval.pure unit).sequenceM [
                  (Eval.pure 1) (Eval.pure 2) (Eval.pure 3)
                ]) {} [1 2 3];
              };

          };
        };

    };
}
