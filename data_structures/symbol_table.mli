exception Already_Exists_In_table
exception CannotExitGlobalScope
exception IncorrectCSTElementsInSymbolTableError
exception Does_Not_Exist_In_Table
class ['a, 'b] table :
  object
    val mutable t : ('a, 'b) Hashtbl.t
    method add : 'a -> 'b -> unit
    method get : 'a -> 'b
    method mem : 'a -> bool
    method set : 'a -> 'b -> unit
  end
type scope =
    Global of (char, Cst.cst) table * scope array
  | Scope of (char, Cst.cst) table * scope array * scope ref
class symboltable :
  object
    val mutable current_scope : scope ref
    val mutable head : scope ref
    method add : Cst.cst -> Cst.cst -> unit
    method enter : unit
    method exit : unit
    method private get_current_table : (char, Cst.cst) table ref
    method set : Cst.cst -> Cst.cst -> unit
  end
