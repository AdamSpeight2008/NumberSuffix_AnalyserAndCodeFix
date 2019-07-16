Imports AdamSpeight.NumberSuffix
Imports AdamSpeight.NumberSuffix.Test.TestHelper
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeFixes
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace Test

  <TestClass>
  Public Class UnitTest
    Inherits CodeFixVerifier

    Private Shared ReadOnly s_Analyzer As New NumberSuffixAnalyzer()
    Private Shared ReadOnly s_CodeFix  As New NumberSuffixCodeFixProvider()

#Region "Overrides"
    Protected Overrides Function GetBasicCodeFixProvider() As CodeFixProvider
      Return s_CodeFix
    End Function

    Protected Overrides Function GetBasicDiagnosticAnalyzer() As DiagnosticAnalyzer
      Return s_Analyzer
    End Function
#End Region

    Shared Friend Function MakeDiagnosticMessage(targetType, sourceType, typeSuffix) As String
      Return String.Format(NumberSuffixAnalyzer.MessageFormat.ToString,targetType, sourceType, typeSuffix )
    End Function

    Private Function MakeTestSource_ForOperatorRight(typeName As String, typeSuffix As String) As String
      Return $"
Module Module1
  Function m(m2 As {typeName}, mask As {typeName}) As Boolean
    Dim fraction As {typeName} = m2 And mask
    Return fraction <> 0{typeSuffix}
  End Function
End Module"
    End Function

    Private Function MakeTestSource_ForOperatorLeft(typeName As String, typeSuffix As String) As String
      Return $"
Module Module1
  Function m(m2 As {typeName}, mask As {typeName}) As Boolean
    Dim fraction As {typeName} = m2 And mask
    Return 0{typeSuffix} <> fraction
  End Function
End Module"
    End Function

    Private Function MakeTestSource_ForCompoundAssignment(typeName As String, typeSuffix As String, fix As Boolean) As String
      Return $"
Module Module1
  Function m() As {typeName}
    Dim fraction As {typeName} = 0{typeSuffix}
    fraction += 1{If(fix,typeSuffix,Nothing)}
    Return fraction
  End Function"
    End Function


<TestMethod>
    Public Sub Test_Empty()
      'No diagnostics expected to show up
      Dim test = ""
      VerifyBasicDiagnostic(test)
    End Sub

    '<DataRow(TargetType, SourceType, TypeSuffix, Line, Column)>
    <DataTestMethod>
    <DataRow("ULong", "Integer", "UL", 5, 12)>
    <DataRow("UInteger", "Integer", "UI", 5, 12)>
    <DataRow("UShort", "Integer", "US", 5, 12)>
    <DataRow("Long", "Integer", "L", 5, 12)>
    <DataRow("Short", "Integer", "S", 5, 12)>
    <DataRow("Decimal", "Integer", "D", 5, 12)>
    <DataRow("Double", "Integer", "R", 5, 12)>
    <DataRow("Single", "Integer", "F", 5, 12)>
    Public Sub Test_Operator_Left(targetTypeName As String, sourceTypeName As String, typeSuffix As String, line As Integer, column As Integer)
      Dim test = MakeTestSource_ForOperatorLeft(targetTypeName, Nothing)
      Dim expected = MakeDiagnosticResult(targetTypeName, sourceTypeName, typeSuffix, line, column)
      VerifyBasicDiagnostic(test, expected)
      Dim fixtest = MakeTestSource_ForOperatorLeft(targetTypeName, typeSuffix)
      VerifyBasicFix(test, fixtest)
    End Sub

    Private Shared Function MakeDiagnosticResult(targetTypeName As String, sourceTypeName As String, typeSuffix As String, line As Integer, column As Integer) As DiagnosticResult
      Return New DiagnosticResult With
                        {.Id = "NumberSuffix",
                          .Message = MakeDiagnosticMessage(targetTypeName, sourceTypeName, typeSuffix),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = {New DiagnosticResultLocation("Test0.vb", line, column)}
                        }
    End Function

    '<DataRow(TargetType, SourceType, TypeSuffix, Line, Column)>
    <DataTestMethod>
    <DataRow("ULong"    ,"Integer", "UL"  ,5  ,24)>
    <DataRow("UInteger" ,"Integer", "UI"  ,5  ,24)>
    <DataRow("UShort"   ,"Integer", "US"  ,5  ,24)>
    <DataRow("Long"     ,"Integer", "L"   ,5  ,24)>
    <DataRow("Short"    ,"Integer", "S"   ,5  ,24)>
    <DataRow("Decimal"  ,"Integer", "D"   ,5  ,24)>
    <DataRow("Double"   ,"Integer", "R"   ,5  ,24)>
    <DataRow("Single"   ,"Integer", "F"   ,5  ,24)>
    Public Sub Test_Operator_Right(targetTypeName As String, sourceTypeName As String, typeSuffix As String, line As Integer, column As Integer )
      Dim test = MakeTestSource_ForOperatorRight(targetTypeName,Nothing)
      Dim expected = MakeDiagnosticResult(targetTypeName, sourceTypeName, typeSuffix, line, column)
      VerifyBasicDiagnostic(test, expected)
      Dim fixtest = MakeTestSource_ForOperatorRight(targetTypeName, typeSuffix)
      VerifyBasicFix(test, fixtest)
    End Sub

    '<DataRow(TargetType, SourceType, TypeSuffix, Line, Column)>
    <DataTestMethod>
    <DataRow("ULong"    ,"Integer"  ,"UL" ,5  ,17)>
    <DataRow("UInteger" ,"Integer"  ,"UI" ,5  ,17)>
    <DataRow("UShort"   ,"Integer"  ,"US" ,5  ,17)>
    <DataRow("Long"     ,"Integer"  ,"L"  ,5  ,17)>
    <DataRow("Short"    ,"Integer"  ,"S"  ,5  ,17)>
    <DataRow("Decimal"  ,"Integer"  ,"D"  ,5  ,17)>
    <DataRow("Double"   ,"Integer"  ,"R"  ,5  ,17)>
    <DataRow("Single"   ,"Integer"  ,"F"  ,5  ,17)>
  Public Sub Test_CompoundAssignment(targetTypeName As String, sourceTypeName As String, typeSuffix As String, line As Integer, column As Integer)
      Dim test = MakeTestSource_ForCompoundAssignment(targetTypeName, typeSuffix, False)
      Dim expected = MakeDiagnosticResult(targetTypeName, sourceTypeName, typeSuffix, line, column)
      VerifyBasicDiagnostic(test, expected)
      Dim fixtest = MakeTestSource_ForCompoundAssignment(targetTypeName, typeSuffix, True)
      VerifyBasicFix(test, fixtest)
    End Sub

#Region "Integer Tests"

    <TestMethod, TestCategory("Integer")>
    Public Sub Test_Integer_CompoundAssignment()
      Dim test = MakeTestSource_ForCompoundAssignment("Integer", "I", false)
      VerifyBasicDiagnostic(test)
    End Sub

    <TestMethod, TestCategory("Integer")>
    Public Sub Test_Integer_Operator_Right()
      Dim test = MakeTestSource_ForOperatorRight("Integer", "I")
      VerifyBasicDiagnostic(test)
    End Sub

    <TestMethod, TestCategory("Integer")>
    Public Sub Test_Integer_Operator_Left()
      Dim test = MakeTestSource_ForOperatorLeft("Integer", "I")
      VerifyBasicDiagnostic(test)
    End Sub

#End Region

  End Class

End Namespace
