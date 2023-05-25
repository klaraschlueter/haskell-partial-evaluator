module FunctionsMM where

import Language.Haskell.TH
import QDefsMM0
import QDefsMM1
import QDefsMM2
import TestFunctions.Build

$qMm

functions = consDec qMm         'mm
          $ consDec qMmColumn   'mmColumn
          $ consDec qMmEntry    'mmEntry
            nilDec

-- Die abstract syntax trees von Listen sind nicht aus den Konstruktoren Cons und Nil in irgendeiner
-- Form aufgebaut, sondern als Liste von Expressions: data Exp = ... | ListE [Exp] | ... .
-- Deshalb wird der Doppelpunkt, der ja eigentlich als Konstruktor Cons aufgefasst werden könnte,
-- als Funktion betrachtet. Eine mit Doppelpunkt verkette Liste ist also während der partiellen
-- Auswertung nicht weiter als Liste verwendbar, solange ich nicht in staticMatch den weiteren
-- hartcodierten Fall einfüge, in dem ein die Infixfunktionsanwendung von (:) im Pattern auf die
-- Infixfunktionsanwendung von (:) im expression matched.
-- erste list of rowvectors, zweite list of columnvectors.
