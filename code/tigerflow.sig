signature tigerflow =
sig

(* structure tigergraph *) (* Estaba en el libro pero no le encontre utilidad *)

type flowgraph

val instr2graph: tigerassem.instr list -> flowgraph * tigergraph.node list (* Toma una lista de instrucciones y devuelve un grafo de flujo *)

end
