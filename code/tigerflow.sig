signature tigerflow =
sig

(* structure tigergraph *) (* Estaba en el libro pero no le encontre utilidad *)

type flowgraph = {control: tigergraph.graph,                               (* Grafo dirigido donde cada nodo representa una instruccion *)
                 def: (tigergraph.node,tigertemp.temp list) Splaymap.dict, (* Tabla de temporarios definidos en cada nodo *)
                 use: (tigergraph.node,tigertemp.temp list) Splaymap.dict, (* Tabla de temporarios usados en cada nodo *)
                 ismove: (tigergraph.node,bool) Splaymap.dict}             (* Indica si cada instruccion es una instruccion move, que podria eliminarse si def y use son iguales *)

val instr2graph: tigerassem.instr list -> flowgraph * tigergraph.node list (* Toma una lista de instrucciones y devuelve un grafo de flujo *)

(* Funciones de debug *)
(* Imprime el grafo *)
val debugGraph: flowgraph * tigergraph.node list -> unit
(* Imprime la informacion de cada nodo(defs, uses, ismove) *)
val debugNodes: flowgraph * tigergraph.node list -> unit
(* Imprime la informacion que corresponde con cada nodo del grafo.
   Esta funcion la usamos para ver a que instruccion corresponde cada nodo
   del grafo de flujo *)
val debugTable: flowgraph * tigergraph.node list -> unit
(* Imprime las 3 cosas de arriba *)
val debug: flowgraph * tigergraph.node list -> unit

end
