package com.mammb.kilo;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static com.mammb.kilo.Interpreter.*;

class LexerTest {

    @Test
    void test001() {
        var lexer = Lexer.of("let five = 5;");
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LET));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "five"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.ASSIGN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.EOF));
    }

    @Test
    void test002() {
        var lexer = Lexer.of("""
                let ten = 10;
                let add = fn(x, y) {
                  x + y;
                };
                let result = add(5, ten);
                """);

        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LET));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "ten"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.ASSIGN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "10"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));

        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LET));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "add"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.ASSIGN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.FN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LPAREN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "x"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.COMMA));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "y"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.RPAREN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LBRACE));

        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "x"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.PLUS));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "y"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));

        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.RBRACE));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));

        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LET));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "result"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.ASSIGN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "add"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LPAREN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.COMMA));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.IDENT, "ten"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.RPAREN));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));
    }

    @Test
    void test003() {
        var lexer = Lexer.of(
                "!-/*5;\n" +
                "5 < 10 > 5;" +
                "5 == 5; 5 != 10;");
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.BANG));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.MINUS));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SLASH));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.ASTER));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));

        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.LT));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "10"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.GT));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));

        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.EQ));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "5"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.NOT_EQ));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.INT, "10"));
        assertThat(lexer.nextToken()).isEqualTo(new Token(TokenType.SEMIC));
    }

}