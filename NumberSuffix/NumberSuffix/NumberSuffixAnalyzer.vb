Imports System
Imports System.Collections.Generic
Imports System.Collections.Immutable
Imports System.Linq
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.Diagnostics

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class NumberSuffixAnalyzer
  Inherits DiagnosticAnalyzer

  Public Const DiagnosticId = "NumberSuffix"

  ' You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
  ' See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
  Private Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.AnalyzerTitle), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Private Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.AnalyzerMessageFormat), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Private Shared ReadOnly Description As New LocalizableResourceString(NameOf(My.Resources.AnalyzerDescription), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Private Const Category = "Naming"

  Private Shared ReadOnly Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault:=True, description:=Description)

  Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
    Get
      Return ImmutableArray.Create(Rule)
    End Get
  End Property

  Public Overrides Sub Initialize(context As AnalysisContext)
    ' TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
    ' See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
    context.RegisterSyntaxNodeAction(AddressOf Analyze, SyntaxKind.NumericLiteralExpression)
  End Sub

  Private Sub Analyze(context As SyntaxNodeAnalysisContext)
    ' TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find
    Dim nme = TryCast( context.Node, LiteralExpressionSyntax)
    If nme Is Nothing Then Exit Sub
    If nme.Kind <> SyntaxKind.NumericLiteralExpression Then Exit Sub
    Dim tkn =nme.Token
    If HasTypeSuffix(tkn) Then Return
    Dim si = context.SemanticModel.GetTypeInfo(nme, context.CancellationToken )
    if si.ConvertedType.GetType.Equals(si.Type) Then exit Sub
    Dim p = nme.Parent
    If p Is Nothing Then exit Sub
    dim pe = TryCast(p, BinaryExpressionSyntax)
    If pe Is Nothing THen Exit Sub
    Dim ct as TypeInfo
    If pe.Left.Equals(nme) Then
       ct = context.SemanticModel.GetTypeInfo(pe.Right, context.CancellationToken)
    Elseif pe.Right.Equals(nme) Then
      ct =  context.SemanticModel.GetTypeInfo(pe.Left, context.CancellationToken)
    Else
      return
    End If
    If ct.Type.Equals(si.Type) THen Exit sub
    ' For all such symbols, produce a diagnostic.
    Dim ts = GetTypeSuffix( ct.Type)
    If ts Is Nothing Then Exit Sub
    Dim diag = Diagnostic.Create(Rule, nme.GetLocation, ts)
    context.ReportDiagnostic(diag)
  End Sub
 private Function HasTypeSuffix(tkn As SyntaxToken) as Boolean
    Dim txt = tkn.Text.ToUpperInvariant()
    Return txt.EndsWith("S")  OrElse txt.EndsWith("I")  OrElse txt.EndsWith("L")  OrElse
           txt.EndsWith("D")  OrElse txt.EndsWith("F")  OrElse txt.EndsWith("R")  OrElse
           txt.EndsWith("US") OrElse txt.EndsWith("UI") OrElse txt.EndsWith("UL")
  End Function
  Friend shared Function GetTypeSuffix(si AS ITypeSymbol) As String
    dim tname=si.ToDisplayString(SymbolDisplayFormat.VisualBasicErrorMessageFormat)
    Select Case tname
           Case "Short"    : Return "S"
           Case "Integer"  : Return "I"
           Case "Long"     : Return "L"
           Case "Decimal"  : Return "D"
           Case "Single"   : Return "F"
           Case "Double"   : Return "R"
           Case "UShort"   : Return "US"
           Case "UInteger" : Return "UI"
           Case "ULong"    : Return "UL"
     End Select
    Return Nothing
  End Function
End Class
