package com.github.natanbc.boolalg.parser

import com.github.natanbc.boolalg.{ Node, SimplificationContext }

trait InfixParselet {
  def parse(ctx: SimplificationContext, parser: Parser, left: Node, token: Token): Node
  
  def precedence: Int
}
