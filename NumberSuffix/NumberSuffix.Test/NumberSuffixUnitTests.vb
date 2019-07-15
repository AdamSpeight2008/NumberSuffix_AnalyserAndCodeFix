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

#Region "Overrides"
    Protected Overrides Function GetBasicCodeFixProvider() As CodeFixProvider
      Return New NumberSuffixCodeFixProvider()
    End Function

    Protected Overrides Function GetBasicDiagnosticAnalyzer() As DiagnosticAnalyzer
      Return New NumberSuffixAnalyzer()
    End Function
#End Region

    <TestMethod>
    Public Sub Test_Empty()
      'No diagnostics expected to show up
      Dim test = ""
      VerifyBasicDiagnostic(test)
    End Sub

#Region "ULong Tests"

    <TestMethod, TestCategory("ULong")>
    Public Sub Test_ULong_Operator_Right()
      Dim test = "
Module Module1
  Function m(m2 As ULong, mask As ULong) As Boolean
    Dim fraction As ULong = m2 And mask
    Return fraction <> 0
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UL"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = {New DiagnosticResultLocation("Test0.vb", 5, 24)}
                        }    


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

    <TestMethod, TestCategory("ULong")>
    Public Sub Test_ULong_Operator_Left()
      Dim test = "
Module Module1
  Function m(m2 As ULong, mask As ULong) As Boolean
    Dim fraction As ULong = m2 And mask
    Return 0 <> fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UL"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = {New DiagnosticResultLocation("Test0.vb", 5, 12)}
                        }    


      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m(m2 As ULong, mask As ULong) As Boolean
    Dim fraction As ULong = m2 And mask
    Return 0UL <> fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("ULong")>
    Public Sub Test_UILong_CompoundAssignment()

      Dim test = "
Module Module1
  Function m() As ULong
    Dim fraction As ULong = 0UL
    fraction += 1
    Return fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UL"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 17) }
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As ULong
    Dim fraction As ULong = 0UL
    fraction += 1UL
    Return fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

#End Region

#Region "UInteger Tests"

    <TestMethod, TestCategory("UInteger")>
    Public Sub Test_UInteger_CompoundAssignment()

      Dim test = "
Module Module1
  Function m() As UInteger
    Dim fraction As UInteger = 0UI
    fraction += 1
    Return fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UI"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 17) }
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As UInteger
    Dim fraction As UInteger = 0UI
    fraction += 1UI
    Return fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("UInteger")>
    Public Sub Test_UInteger_Operator_Right()

      Dim test = "
Module Module1
  Function m() As UInteger
    Dim fraction As UInteger = 1UI
    Return fraction + 1
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UI"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 23)}
                        }

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

    <TestMethod, TestCategory("UInteger")>
    Public Sub Test_UInteger_Operator_Left()

      Dim test = "
Module Module1
  Function m() As UInteger
    Dim fraction As UInteger = 1UI
    Return 1 + fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "UI"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 12)}
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As UInteger
    Dim fraction As UInteger = 1UI
    Return 1UI + fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

#End Region

#Region "UShort Tests"

    <TestMethod, TestCategory("UShort")>
    Public Sub Test_UShort_CompoundAssignment()

      Dim test = "
Module Module1
  Function m() As UShort
    Dim fraction As UShort = 0US
    fraction += 1
    Return fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "US"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 17) }
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As UShort
    Dim fraction As UShort = 0US
    fraction += 1US
    Return fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("UShort")>
    Public Sub Test_UShort_Operator_Right()

      Dim test = "
Module Module1
  Function m() As UShort
    Dim fraction As UShort = 1US
    Return fraction + 1
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "US"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 23)}
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As UShort
    Dim fraction As UShort = 1US
    Return fraction + 1US
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("UShort")>
    Public Sub Test_UShort_Operator_Left()

      Dim test = "
Module Module1
  Function m() As UShort
    Dim fraction As UShort = 1US
    Return 1 + fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "US"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 12)}
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As UShort
    Dim fraction As UShort = 1US
    Return 1US + fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

#End Region

#Region "Long Tests"

    <TestMethod, TestCategory("Long")>
    Public Sub Test_Long_Operator_Right()
      Dim test = "
Module Module1
  Function m(m2 As Long, mask As Long) As Boolean
    Dim fraction As Long = m2 And mask
    Return fraction <> 0
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "L"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = {New DiagnosticResultLocation("Test0.vb", 5, 24)}
                        }    


      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m(m2 As Long, mask As Long) As Boolean
    Dim fraction As Long = m2 And mask
    Return fraction <> 0L
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("Long")>
    Public Sub Test_Long_Operator_Left()
      Dim test = "
Module Module1
  Function m(m2 As Long, mask As Long) As Boolean
    Dim fraction As Long = m2 And mask
    Return 0 <> fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "L"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = {New DiagnosticResultLocation("Test0.vb", 5, 12)}
                        }    


      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m(m2 As Long, mask As Long) As Boolean
    Dim fraction As Long = m2 And mask
    Return 0L <> fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("Long")>
    Public Sub Test_Long_CompoundAssignment()

      Dim test = "
Module Module1
  Function m() As Long
    Dim fraction As Long = 0L
    fraction += 1
    Return fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "L"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 17) }
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As Long
    Dim fraction As Long = 0L
    fraction += 1L
    Return fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

#End Region

#Region "Integer Tests"

    <TestMethod, TestCategory("Integer")>
    Public Sub Test_Integer_CompoundAssignment()

      Dim test = "
Module Module1
  Function m() As Integer
    Dim fraction As Integer = 0I
    fraction += 1
    Return fraction
  End Function
End Module"

      VerifyBasicDiagnostic(test)

    End Sub

    <TestMethod, TestCategory("Integer")>
    Public Sub Test_Integer_Operator_Right()

      Dim test = "
Module Module1
  Function m() As Integer
    Dim fraction As Integer = 1I
    Return fraction + 1
  End Function
End Module"
      'Dim expected As New DiagnosticResult With
      '                  { .Id = "NumberSuffix",
      '                    .Message = String.Format("Do you want to add the type suffix '{0}'?", "I"),
      '                    .Severity = DiagnosticSeverity.Warning,
      '                    .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 23)}
      '                  }

      VerifyBasicDiagnostic(test)

'      Dim fixtest = "
'Module Module1
'  Function m() As Integer
'    Dim fraction As Integer = 1I
'    Return fraction + 1I
'  End Function
'End Module"
'      VerifyBasicFix(test)
    End Sub

    <TestMethod, TestCategory("Integer")>
    Public Sub Test_Integer_Operator_Left()

      Dim test = "
Module Module1
  Function m() As Integer
    Dim fraction As Integer = 1I
    Return 1 + fraction
  End Function
End Module"

      VerifyBasicDiagnostic(test)

    End Sub

#End Region

#Region "UShort Tests"

    <TestMethod, TestCategory("Short")>
    Public Sub Test_Short_CompoundAssignment()

      Dim test = "
Module Module1
  Function m() As Short
    Dim fraction As Short = 0S
    fraction += 1
    Return fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "S"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 17) }
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As Short
    Dim fraction As Short = 0S
    fraction += 1S
    Return fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("Short")>
    Public Sub Test_Short_Operator_Right()

      Dim test = "
Module Module1
  Function m() As Short
    Dim fraction As Short = 1S
    Return fraction + 1
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "S"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 23)}
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As Short
    Dim fraction As Short = 1S
    Return fraction + 1S
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

    <TestMethod, TestCategory("Short")>
    Public Sub Test_Short_Operator_Left()

      Dim test = "
Module Module1
  Function m() As Short
    Dim fraction As Short = 1S
    Return 1 + fraction
  End Function
End Module"
      Dim expected As New DiagnosticResult With
                        { .Id = "NumberSuffix",
                          .Message = String.Format("Do you want to add the type suffix '{0}'?", "S"),
                          .Severity = DiagnosticSeverity.Warning,
                          .Locations = { New DiagnosticResultLocation("Test0.vb", 5, 12)}
                        }

      VerifyBasicDiagnostic(test, expected)

      Dim fixtest = "
Module Module1
  Function m() As Short
    Dim fraction As Short = 1S
    Return 1S + fraction
  End Function
End Module"
      VerifyBasicFix(test, fixtest)
    End Sub

#End Region

  End Class

End Namespace
