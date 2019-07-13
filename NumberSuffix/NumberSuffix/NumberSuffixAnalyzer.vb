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
    context.RegisterSyntaxNodeAction(AddressOf Analyze, SyntaxKind.NumericLiteralExpression)
  End Sub

  Private Sub Analyze(context As SyntaxNodeAnalysisContext)
    Dim literal = TryCast( context.Node, LiteralExpressionSyntax)
    If literal Is Nothing Then Exit Sub
    If literal.Kind <> SyntaxKind.NumericLiteralExpression Then Exit Sub
    Dim tokenFromLiteral =literal.Token
    If HasTypeSuffix(tokenFromLiteral) Then Return
    Dim literalTypeInfo = context.SemanticModel.GetTypeInfo(literal, context.CancellationToken )
    if literalTypeInfo.ConvertedType.GetType.Equals(literalTypeInfo.Type) Then exit Sub
    Dim parentOfLiteral = literal.Parent
    If parentOfLiteral Is Nothing Then exit Sub
    dim parentBinaryExpression = TryCast(parentOfLiteral, BinaryExpressionSyntax)
    If parentBinaryExpression Is Nothing THen Exit Sub
    Dim targetTypeInfo as TypeInfo
    If parentBinaryExpression.Left.Equals(literal) Then
       targetTypeInfo = context.SemanticModel.GetTypeInfo(parentBinaryExpression.Right, context.CancellationToken)
    Elseif parentBinaryExpression.Right.Equals(literal) Then
      targetTypeInfo =  context.SemanticModel.GetTypeInfo(parentBinaryExpression.Left, context.CancellationToken)
    Else
      return
    End If
    If targetTypeInfo.Type.Equals(literalTypeInfo.Type) THen Exit sub
    Dim typeSuffix = GetTypeSuffix( targetTypeInfo.Type)
    If typeSuffix Is Nothing Then Exit Sub
    Dim diag = Diagnostic.Create(Rule, literal.GetLocation, typeSuffix)
    context.ReportDiagnostic(diag)
  End Sub
 private Function HasTypeSuffix(literalToken As SyntaxToken) as Boolean
    Dim literalSuffix = literalToken.Text.ToUpperInvariant()
    Return literalSuffix.EndsWith("S")  OrElse literalSuffix.EndsWith("I")  OrElse literalSuffix.EndsWith("L")  OrElse
           literalSuffix.EndsWith("D")  OrElse literalSuffix.EndsWith("F")  OrElse literalSuffix.EndsWith("R")  OrElse
           literalSuffix.EndsWith("US") OrElse literalSuffix.EndsWith("UI") OrElse literalSuffix.EndsWith("UL")
  End Function
  Friend shared Function GetTypeSuffix(targetTypeSymbol AS ITypeSymbol) As String
    dim typeName= targetTypeSymbol.ToDisplayString(SymbolDisplayFormat.VisualBasicErrorMessageFormat)
    Select Case typeName
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
