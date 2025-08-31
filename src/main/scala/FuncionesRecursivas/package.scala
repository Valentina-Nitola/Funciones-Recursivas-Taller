package FuncionesRecursivas

def maxLin(l: List[Int]): Int =
  if (l.isEmpty) throw new IllegalArgumentException("Lista vacía")
  else if (l.tail.isEmpty) l.head
  else
    val maxTail = maxLin(l.tail)
    if (l.head > maxTail) l.head else maxTail

def maxIt(l: List[Int]): Int =
  if (l.isEmpty) throw new IllegalArgumentException("Lista vacía")
  else
    def aux(lista: List[Int], acc: Int): Int =
      if (lista.isEmpty) acc
      else
        val nuevoAcc = if (lista.head > acc) lista.head else acc
        aux(lista.tail, nuevoAcc)
    aux(l.tail, l.head)
