package com.mammb.kilo;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;
import static com.mammb.kilo.Interpreter.*;

public class ParserTest {

    @Test
    void testLetStatement() {
        String input = """
                let x = 5;
                let foobar = 838383;
                """;
        Statements statements = Parser.of(Lexer.of(input)).parse();

        assertThat(statements.statements().get(0)).isInstanceOf(LetStatement.class);
        LetStatement s0 = (LetStatement) statements.statements().get(0);
        assertThat(s0.token().literal()).isEqualTo("let");
        assertThat(s0.name().value()).isEqualTo("x");
        assertThat(s0.value().toString()).isEqualTo("5");

        assertThat(statements.statements().get(1)).isInstanceOf(LetStatement.class);
        LetStatement s1 = (LetStatement) statements.statements().get(1);
        assertThat(s1.token().literal()).isEqualTo("let");
        assertThat(s1.name().value()).isEqualTo("foobar");
        assertThat(s1.value().toString()).isEqualTo("838383");
    }


    @Test
    void testReturnStatement() {
        String input = """
                return 5;
                return 9090;
                """;
        Statements statements = Parser.of(Lexer.of(input)).parse();

        assertThat(statements.statements().get(0)).isInstanceOf(ReturnStatement.class);
        ReturnStatement s0 = (ReturnStatement) statements.statements().get(0);
        assertThat(s0.token().literal()).isEqualTo("return");
        assertThat(s0.returnValue().toString()).isEqualTo("5");

        assertThat(statements.statements().get(1)).isInstanceOf(ReturnStatement.class);
        ReturnStatement s1 = (ReturnStatement) statements.statements().get(1);
        assertThat(s1.token().literal()).isEqualTo("return");
        assertThat(s1.returnValue().toString()).isEqualTo("9090");
    }


    @ParameterizedTest
    @CsvSource({
            "5 + 5;, 5, +, 5",
            "5 - 5;, 5, -, 5",
            "5 * 5;, 5, *, 5",
            "5 / 5;, 5, /, 5",
            "5 > 5;, 5, >, 5",
            "5 < 5;, 5, <, 5",
            "5 == 5;, 5, ==, 5",
            "5 != 5;, 5, !=, 5",
    })
    void testParsingInfixExpressions(String input, Integer left, String operator, Integer right) {

        Statements statements = Parser.of(Lexer.of(input)).parse();

        assertThat(statements.statements().get(0)).isInstanceOf(ExpressionStatement.class);

        ExpressionStatement s0 = (ExpressionStatement) statements.statements().get(0);

        assertThat(s0.expression()).isInstanceOf(InfixExpression.class);
        InfixExpression e0 = (InfixExpression) s0.expression();
        assertThat(((IntegerLiteral) e0.left()).value()).isEqualTo(left);
        assertThat(e0.operator()).isEqualTo(operator);
        assertThat(((IntegerLiteral)e0.right()).value()).isEqualTo(right);

    }

    @ParameterizedTest
    @CsvSource({
            "-a * b, ((-a) * b)",
            "!-a, (!(-a))",
            "a + b + c, ((a + b) + c)",
            "a + b - c, ((a + b) - c)",
            "a * b * c, ((a * b) * c)",
            "a * b / c, ((a * b) / c)",
            "a + b / c, (a + (b / c))",
            "a + b * c + d / e - f, (((a + (b * c)) + (d / e)) - f)",
            "3 + 4; -5 * 5, (3 + 4)((-5) * 5)",
            "5 > 4 == 3 < 4, ((5 > 4) == (3 < 4))",
            "5 < 4 != 3 > 4, ((5 < 4) != (3 > 4))",
            "3 + 4 * 5 == 3 * 1 + 4 * 5, ((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    })
    void testOperatorPrecedenceParsing(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        assertThat(statements.toString()).isEqualTo(expected);
    }

}
