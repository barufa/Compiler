signature tigercolor =
sig

(* structure Frame : FRAME *)

type allocation = (tigertemp.temp,tigerframe.register) Splaymap.dict

(* Eliminamos los argumentos de color oroginal para dejar mas limpio el codigo del main.
   Asi, cuando se llame a la funcion color estos argumentos se calcularan dentro de la funcion color*)
val color: tigerassem.instr list * tigerframe.frame -> tigerassem.instr list * allocation

val debug: tigerassem.instr list * allocation -> unit

end