type st_entree = { typeof: Cst.cst; is_assigned : bool; is_used : bool; pos:Utils.pos}

exception Already_Exists_In_table of st_entree * st_entree
exception CannotExitGlobalScope
exception IncorrectCSTElementsInSymbolTableError
exception Does_Not_Exist_In_Table of st_entree

class symboltable :
  object
    method add : Cst.cst -> Cst.cst -> unit
    method enter : unit
    method exit : unit
    method assign : Cst.cst -> unit
    method use : Cst.cst -> unit
    method get_type_of : Cst.cst -> Cst.cst
    method warn_on_unused_in_current_scope : unit
    method leave : unit
    method visit : unit
    method get_address : Ast.ast -> Assembly.memory_address
    method get_temp_address : Assembly.memory_address
    method is_in_heap : string -> bool
    method get_heap_address : string -> int
    method reserve_static_space : Ast.ast -> unit
    method update_static_addresses : int -> unit
  end
