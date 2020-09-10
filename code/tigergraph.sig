signature tigergraph =
sig

exception graphEdge
exception notExistNode
exception notExistEdge

type node = int
type graph

val nodes: graph -> node list
val succ: graph -> node -> node list       (* Devuelve la lista de todos los sucesores del nodo *)
val pred: graph -> node -> node list       (* Devuelve la lista de todos los predecesores del nodo *)
val adj: graph -> node -> node list
val eq: node * node -> bool                (* Devuelve true si ambos son el mismo nodo *)

val newGraph: unit -> graph                (* Crea un nuevo grafo vacio *)
val newNode: graph -> graph * node         (* Agrega un nuevo nodo al grafo *)

val isNode: graph -> node -> bool
val isEdge: graph -> {from: node, to: node} -> bool
val isNotEdge: graph -> {from: node, to: node} -> bool

val mk_edge: graph -> {from: node, to: node} -> graph (* Crea la arista de from a to en el grafo *)
val rm_edge: graph -> {from: node, to: node} -> graph (* Elimina la ariste de from a to del grafo *)

(* Esta tabla mapea nodos con lo que representa cada nodo *)
val table: (node,tigerassem.instr) Splaymap.dict ref
(* Funciones para manejar la tabla *)
val createTable: unit -> unit                  (* Inicializa la tabla como vacia *)
val mapNode: node -> tigerassem.instr -> unit  (* Mapea un nuevo nodo con su valor *)
val valNode: node -> tigerassem.instr          (* Devuelve el valor que tiene mapeado el nodo *)

val nodename: node -> string (* for debugging*)

(* Funciones de debug *)
(* Imprime el grafo *)
val debugGraph: graph -> unit
(* Imprime la informacion que corresponde con cada nodo del grafo.
   Esta funcion la usamos para ver a que instruccion corresponde cada nodo
   del grafo de flujo *)
val debugTable: graph -> unit

end
