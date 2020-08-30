package com.mammb.kilo;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;
import static com.mammb.kilo.Interpreter.Parser.*;
import static com.mammb.kilo.Interpreter.*;

public class EvaluatorTest {

    @ParameterizedTest
    @CsvSource({
            "5, 5",
            "10, 10",
            "-10, -10",
            "5 + 2 * 10, 25",
            "(5 + 10 * 2 + 15 / 3) * 2 + -10, 50",
    })
    void testEvalIntegerExpression(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo(expected);
    }


    @ParameterizedTest
    @CsvSource({
            "true, true",
            "false, false",
            "(1 < 2) == true, true",
            "(1 < 2) == false, false",
    })
    void testEvalBooleanExpression(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo(expected);
    }


    @ParameterizedTest
    @CsvSource({
            "!true, false",
            "!false, true",
            "!5, false",
            "!!true, true",
    })
    void testBangOperator(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo(expected);
    }


    @ParameterizedTest
    @CsvSource({
            "if (true) { 10 }, 10",
            "if (false) { 10 }, null",
            "if (1) { 10 }, 10",
            "if (1 < 2) { 10 }, 10",
            "if (1 > 2) { 10 }, null",
            "if (1 > 2) { 10 } else { 20 }, 20",
            "if (1 < 2) { 10 } else { 20 }, 10",
    })
    void testIfElseExpressions(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo(expected);
    }


    @ParameterizedTest
    @CsvSource({
            "return 10;, 10",
            "return 10; 9;, 10",
            "return 2 * 5; 9;, 10",
            "9; return 2 * 5; 9;, 10",
    })
    void testReturnStatements(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo(expected);
    }


    @ParameterizedTest
    @CsvSource({
            "let a = 5; a;, 5",
            "let a = 5 * 5; a;, 25",
            "let a = 5; let b = a; b;, 5",
            "let a = 5; let b = a; let c = a + b + 5; c;, 15",
    })
    void testLetStatements(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo(expected);
    }


    @ParameterizedTest
    @CsvSource({
            "let identity = fn(x) { x; }; identity(5);, 5",
            "let double = fn(x) { x * 2; }; double(5);, 10",
            "'let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));', 20",
            "fn(x) { x; }(5);, 5",
    })
    void testFunctionObject(String input, String expected) {
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo(expected);
    }


    @Test
    void testLetStatement() {
        String input = """
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(3);
            """;
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo("5");
    }

    @Test
    void testHashStatement() {
        String input = """
            let bob = {"name": "Bob", "age": 99};
            bob["name"];
            """;
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo("Bob");
    }

    @Test
    void testArrayHashStatement() {
        String input = """
            let people = [{"name": "Alice", "age": 24}, {"name": "Anna", "age": 28}];
            people[0]["name"];
            """;
        Statements statements = Parser.of(Lexer.of(input)).parse();
        var actual = Evaluator.of().eval(statements);
        assertThat(actual.inspect()).isEqualTo("Alice");
    }

}
