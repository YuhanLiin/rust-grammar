#[macro_export()]
macro_rules! lexer {
    (@rule $token:ident = $re:literal) => {};

    (@re_concat $re1:literal, $( $re:literal, )*) => {
        concat!(
            "(", $re1, ")"
            $( , "|(", $re, ")" ),*
        )
    };

    (@re_concat ) => {""};

    // Check syntax of individual regex before combining them
    (@check $( $re:literal, )*) => {
        $( Regex::new($re).expect(concat!("Token regex ", $re, " has invalid syntax")); )*
    };

    (@one_ignore $ignore:literal) => {1};
    (@one_ignore ) => {0};

    (@has_ignore $ignore:literal $( $stuff:tt )*) => {$( $stuff )*};
    (@has_ignore $( $stuff:tt )*) => {};

    (@mapping $ignore:literal, $( $token:ident )*) => {[None, $( Some(TokenType::$token) ),*]};
    (@mapping $( $token:ident )*) => {[$( Some(TokenType::$token) ),*]};

    (@functions $( $re:literal $( $fn:expr )?, )*) => {[$( lexer!(@func $re $( $fn )? ) ),*]};

    (@func $re:literal $fn:expr) => {Some(Box::new($fn) as FnBox)};
    (@func $re:literal ) => {None};

    (@tok_variant $token:ident $fn:expr) => {$token(T)};
    (@tok_variant $token:ident ) => {$token};

    (@tok_enum_entry $tok_type:ty, $( $tail:tt )*) => {lexer!(@tok_enum $tok_type, $( $tail )*);};
    (@tok_enum_entry , $( $tail:tt )*) => {lexer!(@tok_enum (), $( $tail )*);};

    // Recursive case without a token function
    (@tok_enum $tok_type:ty, $( $token:ident $(($type:ty))?, )* [$new_tok:ident $fn:expr], $( $tail:tt )*) => {
        lexer!(@tok_enum $tok_type, $( $token $(($type))?, )* $new_tok ($tok_type), $( $tail )*);
    };

    // Recursive case with a token function
    (@tok_enum $tok_type:ty, $( $token:ident $(($type:ty))?, )* [$new_tok:ident], $( $tail:tt )*) => {
        lexer!(@tok_enum $tok_type, $( $token $(($type))?, )* $new_tok, $( $tail )*);
    };

    // Final enum generation
    (@tok_enum $tok_type:ty, $( $token:ident $(($type:ty))?, )*) => {
        #[derive(Debug)]
        pub enum Token {
            $( $token $( ($type) )?, )*
        }
    };

    (@tok_type $tok_type:ty) => {$tok_type};
    (@tok_type ) => {()};

    (@tok_val $variant:path, $val:ident, $fn:expr) => {$variant($val.unwrap())};
    (@tok_val $variant:path, $val:ident, ) => {$variant};

    (@tok_placeholder $variant:path, $fn:expr) => {$variant(_)};
    (@tok_placeholder $variant:path, ) => {$variant};

    ($name:ident $(<$tok_type:ty>)?:
     $( [ignore] = $ignore:literal $( -> $ignore_fn:expr )?; )?
     $( $token:ident = $re:literal $( -> $fn:expr )?; )*) => {
        #[allow(unused)]
        #[allow(unused_mut)]
        mod $name {
            use regex::{CaptureMatches, Regex};

            #[derive(Debug, PartialEq, Eq, Clone, Copy)]
            pub enum TokenType {
                $( $token, )*
                __TokenEnd
            }

            lexer!(@tok_enum_entry $($tok_type)?, $( [$token $( $fn )?], )*);

            impl Token {
                fn new(ttype: TokenType, val: Option<lexer!(@tok_type $( $tok_type )?)>) -> Token {
                    match ttype {
                        $( TokenType::$token => lexer!(@tok_val Token::$token, val, $( $fn )?), )*
                        _ => panic!("Should not create token with __TokenEnd")
                    }
                }
            }

            impl From<&Token> for TokenType {
                fn from(tok: &Token) -> TokenType {
                    match *tok {
                        $( lexer!(@tok_placeholder Token::$token, $( $fn )?) => TokenType::$token, )*
                    }
                }
            }

            const TOK_COUNT: usize = TokenType::__TokenEnd as usize;
            const RE_COUNT: usize = TOK_COUNT + lexer!(@one_ignore $($ignore)?);

            // Includes ignore as first element of array
            static TOK_MAPPING: [Option<TokenType>; RE_COUNT] =
                lexer!(@mapping $($ignore,)? $( $token )*);

            type FnBox = Box<FnMut(&str, usize, usize) -> lexer!(@tok_type $($tok_type)?)>;
            type FnPointer = Option<FnBox>;
            type IgnoreBox = Box<FnMut(&str, usize, usize)>;
            type IgnorePointer = Option<IgnoreBox>;

            pub struct Lexer {
                regex: Regex,
                functions: [FnPointer; TOK_COUNT],
                ignore_fn: IgnorePointer
            }

            impl Lexer {
                fn new() -> Self {
                    lexer!(@check $($ignore,)? $($re,)*);
                    let re = lexer!(@re_concat $($ignore,)? $($re,)*);
                    println!("{}", re);
                    let regex = Regex::new(re).unwrap();

                    let functions = lexer!(@functions
                        $( $re $($fn)?, )*
                    );
                    let mut ignore_fn = None;
                    $($( ignore_fn = Some(Box::new($ignore_fn) as IgnoreBox); )?)?
                    Lexer { regex, functions, ignore_fn }
                }

                pub fn lex<'r, 't>(&'r mut self, input: &'t str) -> TokenStream<'r, 't> {
                    TokenStream {
                        captures: self.regex.captures_iter(input),
                        functions: &mut self.functions,
                        ignore_fn: &mut self.ignore_fn,
                    }
                }
            }

            pub struct TokenStream<'r, 't>{
                captures: CaptureMatches<'r, 't>,
                functions: &'r mut [FnPointer; TOK_COUNT],
                ignore_fn: &'r mut IgnorePointer,
            }

            impl<'r, 't> Iterator for TokenStream<'r, 't> {
                type Item = Token;

                fn next(&mut self) -> Option<Self::Item> {
                    loop {
                        if let Some(cap) = self.captures.next() {
                            // Try every possible regex match
                            for i in 0..TOK_MAPPING.len() {
                                if let Some(mat) = cap.get(i + 1) {
                                    let (string, start, end) = (mat.as_str(), mat.start(), mat.end());
                                    // If the match corresponds to an actual token, call the
                                    // corresponding token function and return it
                                    if let Some(tok) = TOK_MAPPING[i] {
                                        let val = self.functions[i - lexer!(@one_ignore $($ignore)?)].as_mut().map(
                                            |f| f(string, start, end));
                                        return Some(Token::new(tok, val));
                                    }
                                    // Otherwise ignore this match and keep consuming
                                    self.ignore_fn.as_mut().map(|f| f(string, start, end));
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

lexer!(test: A = "B";);
lexer!(test1:
       A = "B";
       B = " /t";);
lexer!(test2:
       [ignore] = " ";
       A = "B";);
lexer!(test3:
       [ignore] = " ";);
lexer!(test4:
       );

lexer!(test5 <i32>:
       MUDA = "xyz" -> |_val: &str, _, _| {5};
);

lexer!(joke:
       [ignore] = "/" -> |_val: &str, _, _| {};);
lexer!(joke2 :
       ADAD = "/" -> |_val: &str, _, _| {};);
