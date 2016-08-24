import reductions.LineOfSight

val output = new Array[Float](4)
val input = Array[Float](0f, 1f, 8f, 9f)

0f / 0f

input.tail.scan(0f)((z, f) => math.max(z, f / input.indexOf(f))).toList

LineOfSight.upsweep(input, 1, 4, 1)