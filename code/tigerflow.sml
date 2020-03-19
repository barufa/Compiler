structure tigerflow :> tigerflow = 
struct

open tigerassem

type flowgraph = {control: tigergraph.graph, (* Grafo dirigido donde cada nodo representa una instruccion *)
						     def: (tigergraph.node,tigertemp.temp list) Splaymap.dict, (* Tabla de temporarios definidos en cada nodo *)
						     use: (tigergraph.node,tigertemp.temp list) Splaymap.dict, (* Tabla de temporarios usados en cada nodo *)
						     ismove: (tigergraph.node,bool) Splaymap.dict} (* Indica si cada instruccion es una instruccion move, que podria eliminarse si def y use son iguales *)

fun createFlowGraph instr =
  let
    (* Crea el grafo y agrega 1 nodo por cada elemento de la lista *)
    fun addNodes instr = List.foldl (fn (i,g) => tigergraph.newNode g) (tigergraph.newGraph ()) instr
    (* Agrega las aristas segun el orden de las instrucciones sin tener en cuenta los JUMPs.
       Es decir, crea las aristas (1,2), (2,3), y asi sucesivamente. *)
    fun addEdge graph nodes = foldl (fn ((a,b),g) => tigergraph.mk_edge g {from=a,to=b}) graph (ListPair.zip(List.take(nodes,List.length nodes - 1),List.tl nodes))
    fun auxEJ dic ((IOPER {jump,...},n),g) = (case jump of
                                                SOME l => foldl (fn (m,graph) => tigergraph.mk_edge graph {from=n,to=Splaymap.find(dic,m)}) g l
                                                | NONE => g)
        | auxEJ dic (_,g) = g
    (* Agrega las aristas correspondientes a los JUMPs *)
    fun addEdgeJump graph instr dic = foldl (auxEJ dic) graph instr
    fun createGraph instr =
          let
            val graph = addNodes instr
            val nodes = tigergraph.nodes graph
            fun aux ((ILABEL {lab,...},n),d) = Splaymap.insert(d,lab,n)
                | aux ((_,n),d) = d
            val dic = foldl aux (Splaymap.mkDict String.compare) (ListPair.zip(instr,nodes))
            val graph = addEdge graph nodes
            val graph = addEdgeJump graph (ListPair.zip(instr,nodes)) dic
          in graph
          end
    val g = createGraph instr
    val nodes = tigergraph.nodes g

    fun auxD ((IOPER {dst,...},n),d) = Splaymap.insert(d,n,dst)
        | auxD ((ILABEL _,n),d) = Splaymap.insert(d,n,[])
        | auxD ((IMOVE {dst,...},n),d) = Splaymap.insert(d,n,[dst])
    fun createDef instr = foldl auxD (Splaymap.mkDict Int.compare) (ListPair.zip(instr,nodes))

    fun auxU ((IOPER {src,...},n),d) = Splaymap.insert(d,n,src)
        | auxU ((ILABEL _,n),d) = Splaymap.insert(d,n,[])
        | auxU ((IMOVE {src,...},n),d) = Splaymap.insert(d,n,[src])
    fun createUse instr = foldl auxU (Splaymap.mkDict Int.compare) (ListPair.zip(instr,nodes))

    fun auxM ((IMOVE _,n),d) = Splaymap.insert(d,n,true)
        | auxM ((_,n),d) = Splaymap.insert(d,n,false)
    fun createMove instr = foldl auxM (Splaymap.mkDict Int.compare) (ListPair.zip(instr,nodes))

    val d = createDef instr
    val u = createUse instr
    val m = createMove instr
  in {control = g, def = d,use = u,ismove = m}
  end

fun instr2graph instr =
  let
    val fgraph = createFlowGraph instr
    val nodes = tigergraph.nodes (#control fgraph)
  in (fgraph,nodes)
  end

end
