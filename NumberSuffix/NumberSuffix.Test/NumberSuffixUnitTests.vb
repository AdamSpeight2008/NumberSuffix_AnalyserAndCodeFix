Imports NumberSuffix
Imports NumberSuffix.Test.TestHelper
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeFixes
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.VisualStudio.TestTools.UnitTesting

Namespace NumberSuffix.Test

  <TestClass>
  Public Class UnitTest
    Inherits CodeFixVerifier

    'No diagnostics expected to show up
    <TestMethod>
    Public Sub TestMethod1()
      Dim test = ""
      VerifyBasicDiagnostic(test)
    End Sub

    'Diagnostic And CodeFix both triggered And checked for
    <TestMethod>
    Public Sub TestMethod2()

      Dim test = "
Module Module1
  Function m(m2 As ULong, mask As ULong) As Boolean
    Dim fraction As ULong = m2 And mask
    Return fraction <> 0
  End Function
End Module"
      Dim expected = New DiagnosticResult With {.Id = "NumberSuffix",
          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UL"),
          .Severity = DiagnosticSeverity.Warning,
          .Locations = New DiagnosticResultLocation() {
                  New DiagnosticResultLocation("Test0.vb", 5, 24)
              }}


      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m(m2 As ULong, mask As ULong) As Boolean
    Dim fraction As ULong = m2 And mask
    Return fraction <> 0UL
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod>
    Public Sub TestMethod3()

      Dim test = "
Module Module1
  Function m() As UInteger
    Dim fraction As UInteger = 1UI
    Return fraction + 1
  End Function
End Module"
      Dim expected = New DiagnosticResult With {.Id = "NumberSuffix",
          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UI"),
          .Severity = DiagnosticSeverity.Warning,
          .Locations = New DiagnosticResultLocation() {
                  New DiagnosticResultLocation("Test0.vb", 5, 23)
              }}


      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As UInteger
    Dim fraction As UInteger = 1UI
    Return fraction + 1UI
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

        <TestMethod>
    Public Sub TestMethod4()

      Dim test = "
Module Module1
  Function m() As Integer
    Dim fraction As UInteger = 0UI
    fraction += 1
    Return fraction
  End Function
End Module"
      Dim expected = New DiagnosticResult With {.Id = "NumberSuffix",
          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UI"),
          .Severity = DiagnosticSeverity.Warning,
          .Locations = New DiagnosticResultLocation() {
                  New DiagnosticResultLocation("Test0.vb", 5, 17)
              }}

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As Integer
    Dim fraction As UInteger = 0UI
    fraction += 1UI
    Return fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub
    Protected Overrides Function GetBasicCodeFixProvider() As CodeFixProvider
      Return New NumberSuffixCodeFixProvider()
    End Function

    Protected Overrides Function GetBasicDiagnosticAnalyzer() As DiagnosticAnalyzer
      Return New NumberSuffixAnalyzer()
    End Function

  End Class
End Namespace
