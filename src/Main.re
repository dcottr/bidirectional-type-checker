// Implementation of Bidirectional Typing Rules
// http://davidchristiansen.dk/tutorials/bidirectional.pdf

// Gamma is the context
module StringMap = Map.Make(String);

type t =
  | Bool
  | Function({
      funcIn: t,
      funcOut: t,
    });

/// AST types

type boolean =
  | True
  | False;

type ast =
  | Variable(string)
  | Application({
      func: ast,
      arg: ast,
    })
  | Abstraction({
      x: string,
      body: ast,
    })
  | Boolean(boolean)
  | ConditionalExpression({
      if_expr: ast,
      then_expr: ast,
      else_expr: ast,
    })
  | Annotation(ast, t);

let true_expr = Boolean(True);

let not =
  Annotation(
    Abstraction({
      x: "x",
      body:
        ConditionalExpression({
          if_expr: Variable("x"),
          then_expr: Boolean(False),
          else_expr: Boolean(True),
        }),
    }),
    Function({funcIn: Bool, funcOut: Bool})
  );

let program = Application({func: not, arg: Boolean(True)});

let rec type_to_string = type_ =>
  switch (type_) {
  | Bool => "Bool"
  | Function({funcIn, funcOut}) =>
    "(" ++ type_to_string(funcIn) ++ "-->" ++ type_to_string(funcOut) ++ ")"
  };

let rec ast_to_string = root =>
  switch (root) {
  | Boolean(True) => "True"
  | Boolean(False) => "False"
  | Variable(name) => name
  | ConditionalExpression({if_expr, then_expr, else_expr}) =>
    " if "
    ++ ast_to_string(if_expr)
    ++ ": {"
    ++ ast_to_string(then_expr)
    ++ "} else: {"
    ++ ast_to_string(else_expr)
    ++ "} "
  | Abstraction({x, body}) => "λ" ++ x ++ ".(" ++ ast_to_string(body) ++ ")"
  | Application({func, arg}) =>
    ast_to_string(func) ++ " " ++ ast_to_string(arg)
  | Annotation(expr, type_) =>
    ast_to_string(expr) ++ " : " ++ type_to_string(type_)
  };

print_endline(ast_to_string(program));

let rec check_type = (expr, context, expectedType) =>
  switch (expr) {
  | Abstraction({x, body}) =>
    switch (expectedType) {
    | Function({funcIn, funcOut}) =>
      let context = StringMap.add(x, funcIn, context);
      check_type(body, context, funcOut);
    | type_ =>
      failwith(
        "Expected a function but got "
        ++ type_to_string(type_),
      )
    }
  | _ =>
    let synth_type = synthesize_type(expr, context);
    if (synth_type != expectedType) {
      failwith(
        "Expected type "
        ++ type_to_string(expectedType)
        ++ " but got "
        ++ type_to_string(synth_type),
      );
    };
  }
and synthesize_type = (expr, context) =>
  switch (expr) {
  | Boolean(_) => Bool
  | Variable(name) =>
    switch (StringMap.find_opt(name, context)) {
    | Some(x) => x
    | None => failwith("Used undefined variable " ++ name)
    }
  | Annotation(expr, type_) =>
    check_type(expr, context, type_);
    type_;
  | Application({func, arg}) =>
    switch (synthesize_type(func, context)) {
    | Function({funcIn, funcOut}) =>
      check_type(arg, context, funcIn);
      funcOut;
    | type_ =>
      failwith("Type " ++ type_to_string(type_) ++ " is not a function.")
    }
  | ConditionalExpression({if_expr, then_expr, else_expr}) =>
    check_type(if_expr, context, Bool);
    let t1 = synthesize_type(then_expr, context);
    let t2 = synthesize_type(else_expr, context);
    if (t1 == t2) {
      t1;
    } else {
      failwith(
        "If/else types don't match, success case of type "
        ++ type_to_string(t1)
        ++ " and else of type "
        ++ type_to_string(t2)
        ++ ".",
      );
    };
  | Abstraction(_) => failwith("Couldn't determine the type of lambda.")
  };

print_endline(type_to_string(synthesize_type(not, StringMap.empty)))
