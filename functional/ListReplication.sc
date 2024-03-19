/**
    with each recursive call
    append 1 more copy of the list
    "recover" the original list by calling .toSet
    (!) this is inefficient

    list does not hold duplicate values
    require(arr == arr.toSet)

    f(2, List(2,4,6)) == List(2,2,4,4,6,6)
*/
def f(num:Int, arr:List[Int]) : List[Int] = {
  if (num > 1) f(num-1, arr ++ arr.toSet)
  else arr.sorted
}