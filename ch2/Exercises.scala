object Exercises {
    // 2.1
    def fib(n: Int): Int = {
	@annotation.tailrec
	def fibAux(n: Int, a: Int, b: Int): Int = {
	    if (n == 0) {
		b
	    } else {
		fibAux(n - 1, a + b, a)
	    }
	}
	fibAux(n, 1, 0)
    }

    // 2.2
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
	@annotation.tailrec
	def isSortedAux(as: Array[A], ordered: (A, A) => Boolean,
			n: Int): Boolean =
	{
	    if (n + 1 >= as.length)
		true;
	    else if (ordered(as(n), as(n+1)))
		isSortedAux(as, ordered, n+1);
	    else
		false;
	}
	isSortedAux(as, ordered, 0);
    }

    // 2.3
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
	a => (b => f(a, b))
    }

    // 2.4
    def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
	(a, b) => f(a)(b)
    }
    
    // 2.5
    def compose[A, B, C](f: A => B, g: B => C): A => C = {
	a => g(f(a))
    }
}
