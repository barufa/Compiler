signature tigerliveness =
sig

type igraph = {graph: tigergraph.graph,                              (* Grafo de interferencia *)
              tnode: (tigertemp.temp,tigergraph.node) Splaymap.dict, (* Mapea temporarios del programa assembler a nodos *)
              gtemp: (tigergraph.node,tigertemp.temp) Splaymap.dict, (* Mapeo inverso al anterior, de nodos a temporarios *)
              moves: (tigergraph.node * tigergraph.node) list}       (* Lista de instrucciones moves. Indica que seria bueno asignar a ambos nodos el mismo temporarios si fuera posible *)

val interferenceGraph: tigerflow.flowgraph -> igraph * (tigergraph.node,tigertemp.temp Splayset.set) Splaymap.dict (* Devuelve el grafo de interferencia y un diccionario con cada nodo y sus temporarios que estan liveOut *)

(* Funciones de debug *)
(* Imprime el grafo *)
val debugGraph: igraph -> unit
(* Imprime la informacion de los maps *)
val debugMaps: igraph -> unit
(* Imprime la informacion de los moves *)
val debugMoves: igraph -> unit
(* Imprime las 3 cosas de arriba *)
val debug: igraph -> unit

end
