signature tigergraph =
sig

type graph
type node

val nodes: graph -> node list
val succ: node -> node list (* Devuelve la lista de todos los sucesores del nodo *)
val pred: node -> node list (* Devuelve la lista de todos los predecesores del nodo *)
val adj: node -> node list
val eq: node * node -> bool (* Devuelve true si ambos son el mismo nodo *)

val newGraph: unit -> graph (* Crea un nuevo grafo vacio *)
val newNode: graph -> node  (* Agrega un nuevo nodo al grafo *)
exception GraphEdge
val mk_edge: {from: node, to: node} -> unit (* Crea la arista de from a to en el grafo *)
val rm_edge: {from: node, to: node} -> unit (* Elimina la ariste de from a to del grafo *)

(* Chequear estas 2 lineas*)
structure Table: tigettable
sharing type Table.key = node

val nodename: node -> string (* for debugging*)

end
