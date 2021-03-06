signature tigerframe =
sig

type frame
type register = string

datatype access = InFrame of int | InReg of tigertemp.label
datatype frag = PROC of {body: tigertree.stm, frame: frame}
  | STRING of tigertemp.label * string

(* Algunos registros especiales *)
val rv  : tigertemp.temp
val fp  : tigertemp.temp
val sp  : tigertemp.temp

(* Algunas constantes utiles *)
val fpPrev : int
val fpPrevLev : int
val wSz : int
val log2WSz : int
val argsregs : tigertemp.temp list
val callersaves : tigertemp.temp list
val calleesaves : tigertemp.temp list
val machineregs : tigertemp.temp list
val specialregs : tigertemp.temp list

val newFrame : {name: tigertemp.label, formals: bool list} -> frame
val name : frame -> tigertemp.label
val formals : frame -> access list
val acclist : frame * access-> access
val allocLocal : frame -> bool -> access
val allocArg : frame -> bool -> access
val allocMem : int -> access

val string : tigertemp.label * string -> string
val exp : access -> tigertree.exp -> tigertree.exp
val externalCall : string * tigertree.exp list -> tigertree.exp
val procEntryExit1 : frame * tigertree.stm -> tigertree.stm
val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list

val procEntryExit3 : frame * tigerassem.instr list -> {prolog:string, body: tigerassem.instr list, epilog: string}

end
