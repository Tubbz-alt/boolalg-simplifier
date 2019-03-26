package com.github.natanbc.boolalg.parser

sealed trait TokenKind

object TokenKind {
  case object LeftParen extends TokenKind
  
  case object RightParen extends TokenKind
  
  case object Neg extends TokenKind
  
  case object And extends TokenKind
  
  case object Or extends TokenKind
  
  case object Value extends TokenKind
  
  case object Input extends TokenKind
  
  case object Eof extends TokenKind
}
