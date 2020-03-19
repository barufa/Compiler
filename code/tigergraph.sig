signature tigergraph =
sig

exception graphEdge
exception notExistNode
exception notExistEdge

type node = int
type graph

val nodes: graph -> node list
val succ: graph -> node -> node list (* Devuelve la lista de todos los sucesores del nodo *)
val pred: graph -> node -> node list (* Devuelve la lista de todos los predecesores del nodo *)
val adj: graph -> node -> node list
val eq: node * node -> bool (* Devuelve true si ambos son el mismo nodo *)

val newGraph: unit -> graph (* Crea un nuevo grafo vacio *)
val newNode: graph -> graph  (* Agrega un nuevo nodo al grafo *)

val isNode: graph -> node -> bool
val isEdge: graph -> {from: node, to: node} -> bool
val isNotEdge: graph -> {from: node, to: node} -> bool

val mk_edge: graph -> {from: node, to: node} -> graph (* Crea la arista de from a to en el grafo *)
val rm_edge: graph -> {from: node, to: node} -> graph (* Elimina la ariste de from a to del grafo *)

(* Chequear estas 2 lineas *)
(* Estaban en el libro pero nose que uso tienen *)
(* structure Table: tigettable
sharing type Table.key = node *)

val nodename: node -> string (* for debugging*)

end
