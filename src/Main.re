// Implementation of Bidirectional Typing Rules
// http://davidchristiansen.dk/tutorials/bidirectional.pdf

// Gamma is the context
module StringMap = Map.Make(String);

type t =
  | Bool
  | Function({
      funcIn: t,
      funcOut: t,
    })
  | List(t);

/// AST types

type boolean =
  | True
  | False;

type ast =
  | Boolean(boolean)
  | EmptyList
  | Variable(string)
  | Application({
      func: ast,
      arg: ast,
    })
  | Abstraction({
      x: string,
      body: ast,
    })
  | ConditionalExpression({
      if_expr: ast,
      then_expr: ast,
      else_expr: ast,
    })
  | Annotation(ast, t)
  | Cons({head: ast, rest: ast});

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
let list_ex = Cons({head: Boolean(True), rest: Cons({head: Boolean(True), rest: Cons({head: Boolean(False), rest: EmptyList})})});
let list_of_lists = Cons({head: Annotation(EmptyList, List(Bool)), rest: Cons({head: EmptyList, rest: Cons({head: EmptyList, rest: EmptyList})})});

let rec type_to_string = type_ =>
  switch (type_) {
  | Bool => "Bool"
  | Function({funcIn, funcOut}) =>
    "(" ++ type_to_string(funcIn) ++ "-->" ++ type_to_string(funcOut) ++ ")"
  | List(type_) => "[" ++ type_to_string(type_) ++ "]"
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
  | EmptyList => "[]"
  | Cons({head, rest}) => ast_to_string(head) ++ "::" ++ ast_to_string(rest)
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
  | Cons({head, rest}) =>
    switch(expectedType) {
      | List(type_) =>
        check_type(head, context, type_);
        check_type(rest, context, expectedType)
      | _ => failwith("Can't prepend " ++ ast_to_string(head) ++ " to a non-list: " ++ ast_to_string(rest))
    }
  | EmptyList =>
    switch(expectedType) {
      | List(_) => ()
      | _ => failwith("Expected type " ++ type_to_string(expectedType) ++ " but got " ++ type_to_string(expectedType))
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
  | Cons({head, rest}) =>
    let type_ = synthesize_type(head, context);
    check_type(rest, context, List(type_));
    List(type_)
  | EmptyList => failwith("Couldn't determine the type of empty list.")
  };

print_endline(type_to_string(synthesize_type(not, StringMap.empty)))

print_endline(ast_to_string(list_ex))
print_endline(type_to_string(synthesize_type(list_ex, StringMap.empty)))

print_endline(ast_to_string(list_of_lists))
print_endline(type_to_string(synthesize_type(list_of_lists, StringMap.empty)))
