package com.github.natanbc.boolalg.parser

import com.github.natanbc.boolalg.{ Node, SimplificationContext }

trait PrefixParselet {
  def parse(ctx: SimplificationContext, parser: Parser, token: Token): Node
}
