structure tigerliveness :> tigerliveness = 
struct

type igraph = {graph: tigergraph.graph,													     (* Grafo de interferencia *)
						  tnode: (tigertemp.temp,tigergraph.node) Splaymap.dict, (* Mapea temporarios del programa assembler a nodos *)
						  gtemp: (tigergraph.node,tigertemp.temp) Splaymap.dict, (* Mapeo inverso al anterior, de nodos a temporarios *)
						  moves: (tigergraph.node * tigergraph.node) list}	     (* Lista de instrucciones moves. Indica que seria bueno asignar a ambos nodos el mismo temporarios si fuera posible *)

fun interferenceGraph ({control,def,use,ismove}:tigerflow.flowgraph) = 
  let
    (* definimos una funcion para buscar una key existente en un diccionario *)
    fun findDic dic key = Splaymap.find(dic,key)
    (* funcion que transforma una String list en un String set *)
    fun toSet l = Splayset.addList(Splayset.empty String.compare,l)
    (* Calculamos liveIn y liveOut de cada nodo *)
    val nodes = tigergraph.nodes control
    val inInit = foldr (fn (n,dic) => Splaymap.insert(dic,n,Splayset.empty String.compare)) (Splaymap.mkDict Int.compare) nodes
    val outInit = inInit
    fun update in1 out1 =
      let
        val in2 = in1
        val out2 = out1
        val in1 = foldr (fn (n,dic) => Splaymap.insert(dic,n,Splayset.union(toSet(findDic use n),Splayset.difference(findDic out1 n,toSet(findDic def n))))) (Splaymap.mkDict Int.compare) nodes
        val out1 = foldr (fn (n,dic) => Splaymap.insert(dic,n,foldr (fn (s,set) => Splayset.union(set,findDic in1 s)) (Splayset.empty String.compare) (tigergraph.succ control n))) (Splaymap.mkDict Int.compare) nodes
        (* Compara si el valor de node es igual en ambos diccionarios *)
        fun compare node dic1 dic2 = Splayset.equal(findDic dic1 node,findDic dic2 node)
        val fixedPoint = foldr (fn (n,b) => (compare n in1 in2) andalso (compare n out1 out2) andalso b) true nodes
      in (in1,out1,fixedPoint) end
    fun check (in_,out_,true) = (in_,out_)
      | check (in_,out_,false) = check(update in_ out_)
    val (liveIn,liveOut) = check(inInit,outInit,false)

    (* Calculamos el grafo de interferencia y mapeamos temporarios a nodos y viceversa*)
      (* Calculamos todos los temporarios que se usan *)
    val temps = foldr (fn (n,set) => Splayset.union(Splayset.union(toSet(findDic def n),toSet(findDic use n)),set)) (Splayset.empty String.compare) nodes
    val temps = Splayset.listItems temps
      (* Agregamos los nodos a un grafo vacio y mapeamos temporarios a nodos del grafo utilizando la funcion addNodes *)
    fun addNodes(t,(graph,tnode,gtemp)) = 
      case Splaymap.peek(tnode,t) of
        SOME _ => (graph,tnode,gtemp)
        | NONE => (let 
                    val (g,n) = tigergraph.newNode graph
                   in (g,Splaymap.insert(tnode,t,n),Splaymap.insert(gtemp,n,t)) end)
    val (igraph,tnode,gtemp) = foldl addNodes (tigergraph.newGraph(),Splaymap.mkDict String.compare,Splaymap.mkDict Int.compare) temps
      (* funcion que mapea nodos a temporarios y viceversa *)
      (* mapNode devuelve que temporario le corresponde al nodo n *)
    fun mapNode n = findDic gtemp n
      (* mapTemp devuelve que nodo le corresponde al temprario t *)
    fun mapTemp t = findDic tnode t
      (* Agregamos las aristas del grafo *)
    fun addEdges (node,graph) = foldr (fn (td,g) => foldr (fn (tlo,g') => tigergraph.mk_edge g' {from=mapTemp td,to=mapTemp tlo}) g (Splayset.listItems(findDic liveOut node))) graph (findDic def node)
    val igraph = foldl addEdges igraph nodes

    (* Ahora procesamos los MOVEs *)
      (* funcion que elimina del grafo la arista que no debia ser agregada en el tratamiento especial de los MOVEs(pag 222, punto 2.). 
         Y a su vez se encarga de crear la lista de instrucciones MOVEs. *)
    fun aux (n,(g,l)) = case findDic ismove n of
                        false => (g,l)
                        | true => let
                                    val nodeDef = mapTemp (List.hd (findDic def n))
                                    val nodeUse = mapTemp (List.hd (findDic use n))
                                  in (tigergraph.rm_edge igraph {from=nodeDef,to=nodeUse},(nodeDef,nodeUse)::l) end
    val (igraph,moves) = foldr aux (igraph,[]) nodes

  in ({graph=igraph,tnode=tnode,gtemp=gtemp,moves=moves},liveOut) end

fun debugGraph {graph,tnode,gtemp,moves} =
  let
    fun graphInfo g = tigergraph.debugGraph g
  in
    print("###############################################\n");
    graphInfo graph;
    print("\n")
  end

fun debugMaps {graph,tnode,gtemp,moves} =
  let
    fun mapsInfo n_TO_t =
      let
        val nodes = List.map (#1) (Splaymap.listItems n_TO_t)
        fun show (n::ns) =
          let
            val _ = print("  -Nodo "^tigergraph.nodename n^": "^Splaymap.find(n_TO_t,n)^"\n")
          in show ns end
        | show [] = ()
      in
        show nodes
      end
  in
    print("###############################################\n");
    print("Mostrando mapeos de nodos con temporarios\n");
    print("La informacion se mostrara de la siguiente manera:\n");
    print("-Nodo: Temporario\n");
    mapsInfo gtemp;
    print("\n")
  end

(* Si se intercambian las lineas comentadas por las correspondientes se puede imprimir 
   los moves entre nodos o entre temporarios *)
fun debugMoves {graph,tnode,gtemp,moves} =
  let
    fun movesInfo ((dest,src)::xs) = 
      let
        val _ = print("  "^Splaymap.find(gtemp,dest)^" <-- "^Splaymap.find(gtemp,src)^"\n") (*  *)
        (* val _ = print("  "^tigergraph.nodename dest^" <-- "^tigergraph.nodename src^"\n") *)
      in movesInfo xs end
    | movesInfo [] = ()
  in
    print("###############################################\n");
    print("Mostrando informacion de los moves\n");
    print("La informacion se mostrara de la siguiente manera:\n");
    print("-TempDest <-- TempSrc\n"); (*  *)
    (* val _ = print("-NodeDest <-- NodeSrc\n") *)
    movesInfo moves;
    print("\n")
  end
fun debug igraph =
  let
    val _ = debugMaps igraph
    val _ = debugGraph igraph
    val _ = debugMoves igraph
  in () end

(*Borrar

type igraph = {graph: tigergraph.graph,													     (* Grafo de interferencia *)
						  tnode: (tigertemp.temp,tigergraph.node) Splaymap.dict, (* Mapea temporarios del programa assembler a nodos *)
						  gtemp: (tigergraph.node,tigertemp.temp) Splaymap.dict, (* Mapeo inverso al anterior, de nodos a temporarios *)
						  moves: (tigergraph.node * tigergraph.node) list}	     (* Lista de instrucciones moves. Indica que seria bueno asignar a ambos nodos el mismo temporarios si fuera posible *)
*)
end
