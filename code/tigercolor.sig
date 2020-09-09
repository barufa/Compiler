signature tigercolor =
sig

structure Frame : FRAME

type allocation = (tigertemp.temp,tigerframe.register) Splaymap.dict

(*
val color: {interference: tigerliveness.igraph,   (* grafo de interferencia *)
            initial: allocation,                  (* coloracion inicial de los nodos precoloreados *)
            spillCost: tigergraph.node -> int,    (* spill cost de cada temporario *)
            registers: tigerframe.register list}  (* lista de registros *)
           -> allocation * tigertemp.temp list
*)

(* Eliminamos los argumentos de color oroginal para dejar mas limpio el codigo del main.
   Asi, cuando se llame a la funcion color estos argumentos se calcularan dentro de la funcion color*)
val color: tigerassem.instr list * tigerframe.frame -> allocation * tigertemp.temp list

end