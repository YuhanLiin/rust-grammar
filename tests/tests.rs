#[macro_use]
extern crate syntax;

use std::iter::FromIterator;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Tok {
    NUM,
    PLUS,
    MINUS,
}

// Just testing if this invocation even compiles
grammar!(name_clash <crate::Tok>:
         Symbol = [Rule]
         Rule = [NonTerminal]
         NonTerminal = [Rule]
         Tok = [Symbol]
         Grammar = [Tok]
         grammar = [grammar]
         get_grammar = [get_grammar]);

grammar!(empty_grammar <crate::Tok>:);

grammar!(test_grammar <crate::Tok>:
         Empty = [ | NUM ]
         Stmt = [Expr | ]
         Expr = [NUM | Expr PLUS Expr | Expr MINUS Expr]
);

#[test]
fn empty() {
    use empty_grammar::*;

    let grammar = get_grammar();

    assert_eq!(grammar.iter_lhs().count(), 0);
}

#[test]
fn grammar() {
    use test_grammar::*;

    let grammar = get_grammar();

    let rules = Vec::from_iter(grammar.get_iter_rhs(NonTerminal::Empty));
    assert_eq!(rules.len(), 1);
    assert!(grammar.is_nullable(NonTerminal::Empty));

    let rules = Vec::from_iter(grammar.get_iter_rhs(NonTerminal::Stmt));
    assert_eq!(rules.len(), 1);
    assert_eq!(rules[0].len(), 1);
    assert_eq!(
        rules[0].symbol(0).unwrap(),
        &Symbol::NonTerminal(NonTerminal::Expr)
    );
    assert!(grammar.is_nullable(NonTerminal::Stmt));

    let rules = Vec::from_iter(grammar.get_iter_rhs(NonTerminal::Expr));
    assert_eq!(rules.len(), 3);
    let rules = Vec::from_iter(rules.iter().map(|rule| Vec::from_iter(rule.iter())));
    assert_eq!(rules[0], vec![&Symbol::Terminal(Tok::NUM)]);
    assert_eq!(
        rules[1],
        vec![
            &Symbol::NonTerminal(NonTerminal::Expr),
            &Symbol::Terminal(Tok::PLUS),
            &Symbol::NonTerminal(NonTerminal::Expr),
        ]
    );
    assert_eq!(
        rules[2],
        vec![
            &Symbol::NonTerminal(NonTerminal::Expr),
            &Symbol::Terminal(Tok::MINUS),
            &Symbol::NonTerminal(NonTerminal::Expr),
        ]
    );
}

grammar!(nullable_grammar <crate::Tok>:
         A = [ | PLUS ]
         B = [A]
         C = [B A | NUM]
         D = [PLUS | C NUM B]
         E = [C D]
         F = [A B C | E]
         G = [D | | E E]
         H = [ | D ]
         I = [ E | ]);

#[test]
fn nullable() {
    use nullable_grammar::*;

    let grammar = get_grammar();

    assert!(grammar.is_nullable(NonTerminal::A));
    assert!(grammar.is_nullable(NonTerminal::B));
    assert!(grammar.is_nullable(NonTerminal::C));
    assert!(!grammar.is_nullable(NonTerminal::D));
    assert!(!grammar.is_nullable(NonTerminal::E));
    assert!(grammar.is_nullable(NonTerminal::F));
    assert!(grammar.is_nullable(NonTerminal::G));
    assert!(grammar.is_nullable(NonTerminal::H));
    assert!(grammar.is_nullable(NonTerminal::I));
}
