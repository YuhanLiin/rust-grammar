#[macro_use]
extern crate syntax;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Tok {
    PLUS,
    MINUS,
}

grammar!(null_cycle_grammar <crate::Tok>:
             S = [S S | S S S | ]);

#[test]
#[should_panic(expected = "infinite ambiguity")]
fn nullable_cycle() {
    null_cycle_grammar::get_grammar();
}

grammar!(null_cycle_grammar2 <crate::Tok>:
             S = [ A B ]
             A = [ B ]
             B = [ S | ]);

#[test]
#[should_panic(expected = "infinite ambiguity")]
fn long_nullable_cycle() {
    null_cycle_grammar2::get_grammar();
}

grammar!(cycle_grammar <crate::Tok>:
             Good = [ PLUS | MINUS ]
             Bad = [ Worse ]
             Worse = [ Worst ]
             Worst = [ Good | Bad ]);

#[test]
#[should_panic(expected = "infinite ambiguity")]
fn non_nullable_cycle() {
    cycle_grammar::get_grammar();
}
