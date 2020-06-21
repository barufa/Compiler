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

val color: tigerassem.instr list ->
           {interference: tigerliveness.igraph,   (* grafo de interferencia *)
            initial: allocation,                  (* coloracion inicial de los nodos precoloreados *)
            spillCost: tigergraph.node -> int,    (* spill cost de cada temporario *)
            registers: tigerframe.register list}  (* lista de registros *)
           -> allocation * tigertemp.temp list

end