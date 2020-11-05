structure tigergraph :> tigergraph = 
struct

exception graphEdge
exception notExistNode
exception notExistEdge

type node = int

val table = ref (Splaymap.mkDict Int.compare)

fun createTable () = table := Splaymap.mkDict Int.compare

fun mapNode n v = table := Splaymap.insert(!table,n,v)

fun valNode n = case Splaymap.peek(!table,n) of
                 SOME v => v
                 | NONE => raise Fail "No se encontro el nodo: 21354\n"

type graph = {maxNumNodes: int,
              pred: (node ,node Splayset.set) Splaymap.dict,
              succ: (node ,node Splayset.set) Splaymap.dict}

fun nodes ({succ,...}:graph) = List.map (#1) (Splaymap.listItems succ)

fun succ ({succ,...}:graph) node = 
  case Splaymap.peek(succ,node) of
    SOME value => Splayset.listItems value
    | _        => raise notExistNode

fun pred ({pred,...}:graph) node = 
  case Splaymap.peek(pred,node) of
    SOME value => Splayset.listItems value
    | _        => raise notExistNode

fun eq (node1,node2) = node1 = node2

(* Inicializo la tabla que mapea nodos con lo que representa y luego creo el grafo *)
fun newGraph () = {maxNumNodes = 0,
                   pred = Splaymap.mkDict Int.compare,
                   succ = Splaymap.mkDict Int.compare}

(* Mapeo el nuevo nodo con lo que representa en la tabla y luego lo agrego al grafo *)
fun newNode ({maxNumNodes,pred,succ}:graph) =
  ({maxNumNodes = maxNumNodes + 1,
   pred = Splaymap.insert(pred,maxNumNodes + 1,Splayset.empty Int.compare),
   succ = Splaymap.insert(succ,maxNumNodes + 1,Splayset.empty Int.compare)},
  maxNumNodes+1)

fun isNode ({maxNumNodes,pred,succ}:graph) node =
  let 
    val a = node > 0 andalso node <= (maxNumNodes) (* Compruebo que este entre 0 y maxNumNodes del grafo *)
    val b = case Splaymap.peek(pred,node) of (* Compruebo que exista en el diccionario de predecesores *)
              SOME _ => true
              | NONE => false
    val c = case Splaymap.peek(succ,node) of (* Compruebo que exista en el diccionario de sucesores *)
              SOME _ => true
              | NONE => false
  in if a andalso b andalso c then true else false
  end

fun isEdge ({succ,...}:graph) {from, to} =
  case Splaymap.peek(succ, from) of
    SOME value => Splayset.member(value,to)
    | NONE     => raise notExistNode

fun isNotEdge ({succ,...}:graph) {from, to} = 
  case Splaymap.peek(succ, from) of
    SOME value => (case Splayset.member(value,to) of
                    true    => false
                    | false => true)
    | NONE     => raise notExistNode

fun adj (g as {pred,succ,...}:graph) node = 
  if isNode g node 
  then Splayset.listItems(Splayset.union(Splaymap.find(pred,node),Splaymap.find(succ,node))) (* Chequear *)
  else raise notExistNode

fun mk_edge (g as {maxNumNodes,pred,succ}:graph) {from, to} = 
  if isEdge g {from=from, to=to}
  then g
  else {maxNumNodes = maxNumNodes,
       pred = Splaymap.insert(pred,to,Splayset.add(Splaymap.find(pred,to),from)),
       succ = Splaymap.insert(succ,from,Splayset.add(Splaymap.find(succ,from),to))}

fun rm_edge (g as {maxNumNodes,pred,succ}:graph) {from, to} =
  if isNotEdge g {from=from, to=to}
  then g
  else {maxNumNodes = maxNumNodes,
       pred = Splaymap.insert(pred,to,Splayset.delete(Splaymap.find(pred,to),from)),
       succ = Splaymap.insert(succ,from,Splayset.delete(Splaymap.find(succ,from),to))}

fun nodename node = Int.toString node

fun debugGraph g =
  let
    val _ = print("Imprimiendo informacion del grafo\n")
    val _ = print("La informacion se mostrara de la siguiente manera:\n")
    val _ = print("-Nodo\n  -Predecesores\n  -Sucesores\n\n")
    fun infoNode n =
      let
        val _ = print("  -Nodo "^nodename n^":\n")
        fun f a = print(nodename a^",")
        val _ = print("    -Pred: ")
        val _ = List.app f (pred g n)
        val _ = print("\n")
        val _ = print("    -Succ: ")
        val _ = List.app f (succ g n)
        val _ = print("\n")
      in () end
  in
    List.app infoNode (nodes g)
  end

fun debugTable g =
  let
    val _ = print("Imprimiendo informacion de la tabla\n")
    val _ = print("La informacion se mostrara de la siguiente manera:\n")
    val _ = print("-Nodo: Instruccion\n\n")
    fun infoTable n =
      let
        val _ = print("  Nodo "^nodename n^": ")
        val _ = tigerassem.formatCode (valNode n)
      in () end
  in
    List.app infoTable (nodes g)
  end

end