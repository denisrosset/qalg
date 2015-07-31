package com.faacets.qalg
package math

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.NoImplicit
import spire.algebra._
import spire.syntax.cfor._

import indup.algebra._

import algebra._

sealed trait Matrix[@sp(Double, Long) A, IM <: ImmMut] {
  implicit def classTagA: ClassTag[A]
  implicit def zeroA: Zero[A]
  override def toString = impl.Print.print(nRows, nCols, (r, c) => apply(r, c).toString)
  def dense: DenseMatrix[A, IM]

  def apply(r: Int, c: Int): A
  def update(r: Int, c: Int, newA: A)(implicit ev: IM =:= Mut): Unit
  def nRows: Int
  def nCols: Int
  def offsetHead: OptOffset
  def offsetNext(offset: Offset): OptOffset
  def offsetValue(offset: Offset): A
  def offsetX(offset: Offset): IntInt
  def offsetX0(offset: Offset): Int
  def offsetX1(offset: Offset): Int
  def feedTo(builder: MatBuilder[_, A]): Unit
  def copy: Matrix[A, IM]
}

trait MatrixLowPriority {
  implicit def MatrixMatRing[@sp(Long) A: Ring: Zero: ClassTag, IM <: ImmMut]: MatRing[Matrix[A, IM], A] with MatSlice[Matrix[A, IM], Vector[A, IM], A] { type Options = Unit } = new MatrixMatRing[A, IM] 
}

object Matrix extends MatrixLowPriority {
  def apply[@sp(Double, Long) A: Zero: ClassTag, IM <: ImmMut](implicit A: Ring[A], ev: NoImplicit[Field[A]]): MatRing[Matrix[A, IM], A] = MatrixMatRing[A, IM]
  def apply[@sp(Double, Long) A: Zero: ClassTag, IM <: ImmMut](implicit A: Field[A]): MatField[Matrix[A, IM], A] = MatrixMatFieldSlice[A, IM]

  implicit def MatrixMatFieldSlice[@sp(Double) A: Field: Zero: ClassTag, IM <: ImmMut]: MatField[Matrix[A, IM], A] with MatSlice[Matrix[A, IM], Vector[A, IM], A] { type Options = Unit } = new MatrixMatField[A, IM]
  implicit def MatrixMatVecProduct[@sp(Double, Long) A, IM <: ImmMut](implicit M: MatRing[Matrix[A, IM], A], V: VecRing[Vector[A, IM], A]): MatVecProduct[Matrix[A, IM], Vector[A, IM]] = new MatrixMatVecProduct[A, IM] 
  trait Tag[IM <: ImmMut]
  implicit def MatrixUpdate[@sp(Double, Long) A](implicit M: Index2[Matrix[A, Mut], Matrix[A, Mut], Vector[A, Mut], A]): Update2[Matrix[A, Mut], A] = new MatrixUpdate[A]
  implicit def Base[@sp(Double, Long) A, IM <: ImmMut]: Base[Matrix[A, IM], Tag[IM]] = null
  implicit def Refine[@sp(Double, Long) A, IM <: ImmMut]: Refine[Tag[IM], A, Matrix[A, IM]] = null
  object packs {
    implicit def PackFM[@sp(Double) A: Eq: ClassTag: Pivot](implicit M: MatField[Matrix[A, Mut], A] with MatSlice[Matrix[A, Mut], Vector[A, Mut], A], V: VecField[Vector[A, Mut], A], MV: MatVecProduct[Matrix[A, Mut], Vector[A, Mut]], MM: MatMut[Matrix[A, Mut], A], VM: VecMut[Vector[A, Mut], A]): algos.PackFieldMutable.ForMV[Matrix[A, Mut], Vector[A, Mut], A] = new PackFM[A]
    implicit def PackFI[@sp(Double) A: Eq: ClassTag: Pivot](implicit M: MatField[Matrix[A, Imm], A] with MatSlice[Matrix[A, Imm], Vector[A, Imm], A], V: VecField[Vector[A, Imm], A], MV: MatVecProduct[Matrix[A, Imm], Vector[A, Imm]], mutablePack: algos.PackFieldMutable.ForMV[Matrix[A, Mut], Vector[A, Mut], A]): algos.PackField.ForMV[Matrix[A, Imm], Vector[A, Imm], A] = new PackF[A, Imm]
  }
}

final class DenseMatrix[@sp(Double, Long) A, IM <: ImmMut](val nRows: Int, val nCols: Int, val data: Array[A])(implicit val V: MatBuild[Matrix[A, IM], A], val classTagA: ClassTag[A], val zeroA: Zero[A]) extends Matrix[A, IM] {
  def dense: DenseMatrix[A, IM] = this

  def apply(r: Int, c: Int): A = data(nRows * c + r)
  def update(r: Int, c: Int, newA: A)(implicit ev: IM =:= Mut): Unit = data(nRows * c + r) = newA
  def copy: DenseMatrix[A, IM] = new DenseMatrix[A, IM](nRows, nCols, data.clone)
  def feedTo(builder: MatBuilder[_, A]): Unit = impl.MatDense.feedTo[Matrix[A, IM], A](this, builder)
//  def offsetHead: OptOffset = if (data.length == 0) NoOffset else Offset(0)
//  def offsetNext(offset: Offset): OptOffset = if (offset.o + 1 >= data.length) NoOffset else Offset(offset.o + 1)
//  def offsetValue(offset: Offset): A = data(offset.o.toInt)
  def offsetHead: OptOffset = if (nRows > 0 && nCols > 0) Offset(0L) else NoOffset
  def offsetNext(offset: Offset): OptOffset = {
    val r = offsetX0(offset)
    val c = offsetX1(offset)
    if (r + 1 < nRows)
      Offset(IntInt.encode(r + 1, c))
    else if (c + 1 < nCols)
      Offset(IntInt.encode(0, c + 1))
    else
      NoOffset
  }
  def offsetValue(offset: Offset): A = apply(offsetX0(offset), offsetX1(offset))
  def offsetX0(offset: Offset): Int = IntInt.lowInt(offset.o)
  def offsetX1(offset: Offset): Int = IntInt.highInt(offset.o)
  def offsetX(offset: Offset): IntInt = new IntInt(offset.o)
}
