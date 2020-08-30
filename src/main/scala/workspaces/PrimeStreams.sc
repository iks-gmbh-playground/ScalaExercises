def primeStream(ls: LazyList[Int]): LazyList[Int] =
  ls.head #:: primeStream(ls.tail.filter(_ % ls.head != 0))

val primes = primeStream(LazyList.from(2))

primes.take(10).toList

val ps: LazyList[Int] = 2 #:: LazyList.from(3)
  .filter(i => ps.takeWhile(p => p * p <= i).forall(p => i % p > 0))

ps.take(10).toList
