package com.eddsteel

package object advent {
  type AdventErrorOr[A] = Either[AdventError, A]
}
