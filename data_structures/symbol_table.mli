exception Already_Exists_In_table
exception CannotExitGlobalScope
exception IncorrectCSTElementsInSymbolTableError
exception Does_Not_Exist_In_Table

class symboltable :
  object
    method add : Cst.cst -> Cst.cst -> unit
    method enter : unit
    method exit : unit
    method assign : Cst.cst -> unit
    method use : Cst.cst -> unit
    method get_type_of : Cst.cst -> Cst.cst
  end
