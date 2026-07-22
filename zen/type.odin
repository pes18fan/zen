package zen

import "core:fmt"
import "core:slice"

// A type that may either be monomorphized (only one variant) or a generic
// type instantiated per use.
TypeScheme :: union #no_nil {
	Type,
	TypeQuantified,
}

// A monomorphized type. May be a type variable that can bind to something in
// the future, a concrete type function application, the `Any` type, or the
// `Never` type.
Type :: union #no_nil {
	TypeVariable,
	TypeFunctionApplication,
	TypeAny,
	TypeNever,
}

// The `Any` type represents a value whose type information has intentionally
// been erased. It unifies with anything.
TypeAny :: struct {}

// The `Never` type represents the type of an expression that can never exist,
// or that of an expression that never completes executing. It unifies with
// anything when it is assigned to some other type or in branching statements,
// and in other cases it behaves like a regular type function application.
TypeNever :: struct {}

// A type variable with a unique identifier; it is used to represent the type
// of something that we have not inferred completely yet. It can unify with
// other type variables or concrete types (except `Never`) to turn into those
// types.
TypeVariable :: struct {
	// identifier for the variable
	idx: int,
}

// Represents a concrete type. The "type function" is the notion of a type
// constructor, i.e. a function that takes types as parameters and returns another
// type. Hence, a "type function application" is a concrete type created by
// "applying" (calling) this constructor with some type arguments.
//
// It consists of the specific type constructor itself, and a slice of arguments
// to that constructor.
//
// Most type function applications in zen are nullary, i.e. they do not take
// any arguments and are just applications by themselves. For example, `Number`,
// `Nil`, `Bool` and `String` are all nullary.
//
// `List` and `Result` are two type function applications that take a fixed
// number of arguments; `List[T]` takes in the one argument `T` that is the type
// of the list's values, and `Result[T, E]` takes in two arguments `T` and `E`
// which are the types of the ok and err values.
//
// The function type constructor takes in a variable number of arguments. A
// function type is `(T, U, ...) -> R` where `T, U, ...` is a list of zero or
// more type arguments representing the parameters of the function, and `R`
// is its return type. This syntax is the user-facing string representation
// of the function type; in the implementation it is more akin to something
// like `Function[T, U, ..., R]`; all but the last type of the slice of
// type args represent the parameter types to the function, and the last type
// `R` (which is mandatory unlike the parameters) is the return type.
TypeFunctionApplication :: struct {
	constructor: TypeConstructor,
	args:        []Type,
}

// Represents a Hindley-Milner polytype; a monomorphic type with a top-level
// `forall` quantifier over some bound type variables. For example, the type
// `forall a. List[a]` represents the notion of "any list".
TypeQuantified :: struct {
	bound: []TypeVariable,
	type:  Type,
}

// A fixed set of type constructors built-in to zen.
TypeConstructor :: enum {
	NIL, // perhaps replace with UNIT ()
	BOOL,
	NUMBER,
	STRING,
	FUNCTION,
	LIST,
	RESULT,
	RECORD, // to be added
}

type_constructor_string :: proc(c: TypeConstructor) -> string {
	switch c {
	case .NIL:
		return "Nil"
	case .BOOL:
		return "Bool"
	case .NUMBER:
		return "Number"
	case .STRING:
		return "String"
	case .FUNCTION:
		return "Func"
	case .LIST:
		return "List"
	case .RESULT:
		return "Result"
	case .RECORD:
		return "Record"
	}

	fmt.panicf("invalid type constructor %v", c)
}

// Create a fresh type variable.
fresh :: proc {
	fresh_typechecker,
	fresh_resolver,
}

fresh_typechecker :: #force_inline proc(tc: ^TypeChecker) -> TypeVariable {
	idx := tc.typevar_count
	var := TypeVariable{idx}
	when ODIN_DEBUG {
		if opt.log_checker {
			fmt.eprintfln("-- create fresh type variable %v", type_string(var, true))
		}
	}

	tc.typevar_count += 1
	return var
}

fresh_resolver :: #force_inline proc(rs: ^Resolver) -> TypeVariable {
	idx := rs.typevar_count
	var := TypeVariable{idx}
	when ODIN_DEBUG {
		if opt.log_checker {
			fmt.eprintfln("-- create fresh type variable %v", type_string(var, true))
		}
	}

	rs.typevar_count += 1
	return var
}

// Create a type function application, with the given `constructor` and `args`.
// If `args` is not nil and is valid for the provided `constructor`, it is
// **cloned using the current `context.allocator`**.
tapp :: proc(constructor: TypeConstructor, args: []Type = nil) -> TypeFunctionApplication {
	switch constructor {
	case .NUMBER, .NIL, .BOOL, .STRING:
		return TypeFunctionApplication{constructor = constructor, args = nil}
	case .FUNCTION:
		assert(args != nil, "cannot have nil type args for a function type")

		// a function type here may take zero or more values and returns exactly
		// one; hence at least one type arg.
		// for instance a function like func() {} takes nothing and returns unit
		assert(len(args) > 0, "must have at least one (return type) arg for a function type")

		// cloning the slice is a bit bad for performance but it keeps `tapp` reliable
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .LIST:
		assert(args != nil, "cannot have nil type args for a list type")
		assert(len(args) == 1, "must have one type arg exactly for a list type")
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .RESULT:
		assert(args != nil, "cannot have nil type args for a result type")
		assert(len(args) == 2, "must have two type args exactly for a result type")
		return TypeFunctionApplication{constructor = constructor, args = slice.clone(args)}
	case .RECORD:
		unimplemented()
	}

	fmt.panicf("invalid type constructor %v", constructor)
}

type_any :: TypeAny{}
type_never :: TypeNever{}

// Create a `forall` quantified form of `type` over the given variables in
// `bound`.
tquant :: #force_inline proc(bound: []TypeVariable, type: Type) -> TypeQuantified {
	return TypeQuantified{bound = slice.clone(bound), type = type}
}

@(require_results)
is_type_variable :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeVariable)
	return ok
}

@(require_results)
is_type_function_application :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeFunctionApplication)
	return ok
}

@(require_results)
is_type_any :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeAny)
	return ok
}

@(require_results)
is_type_never :: #force_inline proc(ty: Type) -> bool {
	_, ok := ty.(TypeNever)
	return ok
}

as_type_variable :: #force_inline proc(ty: Type) -> TypeVariable {
	return ty.(TypeVariable)
}

as_type_function_application :: #force_inline proc(ty: Type) -> TypeFunctionApplication {
	return ty.(TypeFunctionApplication)
}

as_type_any :: #force_inline proc(ty: Type) -> TypeAny {
	return ty.(TypeAny)
}

as_type_never :: #force_inline proc(ty: Type) -> TypeNever {
	return ty.(TypeNever)
}
