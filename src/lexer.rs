#[macro_export()]
macro_rules! lexer {
    (@rule $token:ident = $re:expr) => {};

    (@re_concat $re1:expr, $( $re:expr, )*) => {
        concat!(
            "(", $re1, ")"
            $( , "|(", $re, ")" ),*
        )
    };

    (@re_concat ) => {""};

    // Check syntax of individual regex before combining them
    (@check $( $re:expr, )*) => {
        $( Regex::new($re).expect(concat!("Token regex ", $re, " has invalid syntax")); )*
    };

    (@has_ignore $ignore:expr ) => {1};
    (@has_ignore ) => {0};

    (@mapping $ignore:expr, $( $token:ident )*) => {[None, $( Some(Token::$token) ),*]};
    (@mapping $( $token:ident )*) => {[$( Some(Token::$token) ),*]};

    ($name:ident:
     $( [ignore] = $ignore:expr; )?
     $( $token:ident = $re:expr; )*) => {
        mod $name {
            use regex::{CaptureMatches, Regex};

            #[derive(Debug, PartialEq, Eq, Clone, Copy)]
            pub enum Token {
                $( $token, )*
                __TokenEnd
            }

            const TOK_COUNT: usize = Token::__TokenEnd as usize;

            // Includes ignore as first element of array
            static TOK_MAPPING: [Option<Token>; TOK_COUNT + lexer!(@has_ignore $($ignore)?)] =
                lexer!(@mapping $($ignore,)? $( $token )*);

            pub struct Lexer {
                regex: Regex,
            }

            impl Lexer {
                fn new() -> Self {
                    lexer!(@check $($ignore,)? $($re,)*);
                    let re = lexer!(@re_concat $($ignore,)? $($re,)*);
                    println!("{}", re);
                    let regex = Regex::new(re).unwrap();
                    Lexer { regex }
                }

                pub fn lex<'r, 't>(&'r self, input: &'t str) -> TokenStream<'r, 't> {
                    TokenStream(self.regex.captures_iter(input))
                }
            }

            pub struct TokenStream<'r, 't>(CaptureMatches<'r, 't>);

            impl<'r, 't> Iterator for TokenStream<'r, 't> {
                type Item = Token;

                fn next(&mut self) -> Option<Self::Item> {
                    loop {
                        if let Some(cap) = self.0.next() {
                            // Try every possible regex match
                            for i in 0..TOK_MAPPING.len() {
                                if let Some(_mat) = cap.get(i + 1) {
                                    // If the match corresponds to an actual token, return it
                                    if let Some(tok) = TOK_MAPPING[i] {
                                        return Some(tok);
                                    }
                                    // Otherwise ignore this match and keep consuming
                                    break;
                                }
                            }
                        } else {
                            // No more tokens
                            return None
                        }
                    }
                }
            }
        }
    };
}

lexer!(test: a = "B";);
