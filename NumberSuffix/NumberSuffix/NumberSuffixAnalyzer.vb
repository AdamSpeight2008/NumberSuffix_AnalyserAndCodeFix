Imports System
Imports System.Collections.Generic
Imports System.Collections.Immutable
Imports System.Linq
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Microsoft.CodeAnalysis.Diagnostics
Imports System.Runtime.CompilerServices

<Assembly: InternalsVisibleTo("NumberSuffix.Test")>

Friend Module Exts

  <Extension>
  Function NOR(b0 As Boolean, b1 As Boolean) As Boolean
    Return Not (b0 OrElse b1)
  End Function

End Module

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class NumberSuffixAnalyzer
  Inherits DiagnosticAnalyzer

  Friend Const DiagnosticId = "NumberSuffix"
  Friend Const Category = "Naming"
  Friend Shared ReadOnly MessageFormat As New LocalizableResourceString(NameOf(My.Resources.AnalyzerMessageFormat), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Shared ReadOnly Title As New LocalizableResourceString(NameOf(My.Resources.AnalyzerTitle), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Shared ReadOnly Description As New LocalizableResourceString(NameOf(My.Resources.AnalyzerDescription), My.Resources.ResourceManager, GetType(My.Resources.Resources))
  Shared ReadOnly Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault:=True, description:=Description)

  Public Function MessageFormatted() As String
    Return MessageFormat.ToString
  End Function

  Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
    Get
      Return ImmutableArray.Create(Rule)
    End Get
  End Property

  Public Overrides Sub Initialize(context As AnalysisContext)
    context.RegisterSyntaxNodeAction(AddressOf Analyze, SyntaxKind.NumericLiteralExpression)
  End Sub

  Private Sub Analyze(context As SyntaxNodeAnalysisContext)
    Dim literal = TryCast(context.Node, LiteralExpressionSyntax)
    Dim targetTypeInfo As TypeInfo
    Dim typeSuffix As String = Nothing
    Dim literalTypeInfo As TypeInfo
    If literal Is Nothing OrElse literal.Kind <> SyntaxKind.NumericLiteralExpression OrElse HasTypeSuffix(literal.Token) Then Return

    literalTypeInfo = context.SemanticModel.GetTypeInfo(literal, context.CancellationToken)
    If literalTypeInfo.ConvertedType.GetType.Equals(literalTypeInfo.Type) Then Return
    If literal.Parent Is Nothing Then Return
    If TryGetTypeFromParentBinaryExpression(literal.Parent, literal, context.SemanticModel, context.CancellationToken, targetTypeInfo).NOR(
       TryGetTypeInfoFromParentAssignment(literal.Parent, context.SemanticModel, context.CancellationToken, targetTypeInfo)) Then Return
    If targetTypeInfo.Type.Equals(literalTypeInfo.Type) Then Return
    If Not TryGetTypeSuffix(targetTypeInfo.Type, typeSuffix) Then Return
    context.ReportDiagnostic(Diagnostic.Create(Rule, literal.GetLocation, targetTypeInfo.Type, literalTypeInfo.Type, typeSuffix))
  End Sub

  Private Function HasTypeSuffix(literalToken As SyntaxToken) As Boolean
    Dim literalSuffix = literalToken.Text.ToUpperInvariant()
    Return literalSuffix.EndsWith("S")  OrElse literalSuffix.EndsWith("I")  OrElse literalSuffix.EndsWith("L")  OrElse
           literalSuffix.EndsWith("D")  OrElse literalSuffix.EndsWith("F")  OrElse literalSuffix.EndsWith("R")  OrElse
           literalSuffix.EndsWith("US") OrElse literalSuffix.EndsWith("UI") OrElse literalSuffix.EndsWith("UL")
  End Function

  Friend Shared Function TryGetTypeSuffix(targetTypeSymbol As ITypeSymbol, ByRef typeSuffix As String) As Boolean
    Dim typeName = targetTypeSymbol.ToDisplayString(SymbolDisplayFormat.VisualBasicErrorMessageFormat)
    Select Case typeName
           Case "Short"    : typeSuffix = "S"
           Case "Integer"  : typeSuffix = "I"
           Case "Long"     : typeSuffix = "L"
           Case "Decimal"  : typeSuffix = "D"
           Case "Single"   : typeSuffix = "F"
           Case "Double"   : typeSuffix = "R"
           Case "UShort"   : typeSuffix = "US"
           Case "UInteger" : typeSuffix = "UI"
           Case "ULong"    : typeSuffix = "UL"
           Case Else       : typeSuffix = Nothing
    End Select
    Return typeSuffix IsNot Nothing
  End Function

  Friend Shared Function TryGetTypeFromParentBinaryExpression(parentOfLiteral As SyntaxNode,
                                                              literal As LiteralExpressionSyntax,
                                                              sm As SemanticModel,
                                                              ct As CancellationToken,
                                                        ByRef targetTypeInfo As TypeInfo
                                                            ) As Boolean
    Dim parentBinaryExpression = TryCast(parentOfLiteral, BinaryExpressionSyntax)
    If parentBinaryExpression Is Nothing Then Return False
    If parentBinaryExpression.Left.Equals(literal)  Then targetTypeInfo = sm.GetTypeInfo(parentBinaryExpression.Right, ct)  : Return True
    IF parentBinaryExpression.Right.Equals(literal) Then targetTypeInfo = sm.GetTypeInfo(parentBinaryExpression.Left, ct)   : Return True
    Return False
  End Function

  Friend Shared Function TryGetTypeInfoFromParentAssignment(parentOfLiteral As SyntaxNode,
                                                            sm As SemanticModel,
                                                            ct As CancellationToken,
                                                      ByRef targetTypeInfo As TypeInfo
                                                          ) As Boolean
    Dim parentAssignment = TryCast(parentOfLiteral, AssignmentStatementSyntax)
    If parentAssignment Is Nothing OrElse parentAssignment.Left Is Nothing Then Return False ' += 1; eg no assignment target.
    Select Case parentAssignment.Kind
           Case SyntaxKind.AddAssignmentStatement,
                SyntaxKind.SubtractAssignmentStatement,
                SyntaxKind.DivideAssignmentStatement,
                SyntaxKind.IntegerDivideAssignmentStatement,
                SyntaxKind.MultiplyAssignmentStatement,
                SyntaxKind.ConcatenateAssignmentStatement,
                SyntaxKind.ExponentiateAssignmentStatement,
                SyntaxKind.LeftShiftAssignmentStatement,
                SyntaxKind.RightShiftAssignmentStatement
           Case Else
                targetTypeInfo = Nothing : Return False
    End Select
    targetTypeInfo = sm.GetTypeInfo(parentAssignment.Left, ct)
    Return True
  End Function

End Class
