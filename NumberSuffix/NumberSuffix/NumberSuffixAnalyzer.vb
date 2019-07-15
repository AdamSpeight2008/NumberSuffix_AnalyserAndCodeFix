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
    If literal Is Nothing OrElse literal.Kind <> SyntaxKind.NumericLiteralExpression Then Exit Sub
    Dim tokenFromLiteral =literal.Token
    If HasTypeSuffix(tokenFromLiteral) Then Return
    Dim literalTypeInfo = context.SemanticModel.GetTypeInfo(literal, context.CancellationToken )
    if literalTypeInfo.ConvertedType.GetType.Equals(literalTypeInfo.Type) Then exit Sub
    Dim parentOfLiteral = literal.Parent
    Dim targetTypeInfo as TypeInfo
    If parentOfLiteral Is Nothing Then Return
    If TryGetTypeFromParentBinaryExpression(parentOfLiteral,literal,context.SemanticModel,context.CancellationToken,targetTypeInfo) Then
    ElseIf TryGetTypeInfoFromParentAssignment(parentOfLiteral, context.SemanticModel, context.CancellationToken, targetTypeInfo) Then
    Else
      exit Sub
    End If
    
    If targetTypeInfo.Type.Equals(literalTypeInfo.Type) THen Exit sub
    Dim typeSuffix = GetTypeSuffix( targetTypeInfo.Type)
    If typeSuffix Is Nothing Then Exit Sub
    Dim diag = Diagnostic.Create(Rule, literal.GetLocation, targetTypeInfo.Type, literalTypeInfo.Type, typeSuffix)
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

  Friend Shared Function TryGetTypeFromParentBinaryExpression(parentOfLiteral As SyntaxNode ,
                                                              literal as LiteralExpressionSyntax,
                                                              sm as SemanticModel,
                                                              ct As CancellationToken,
                                                        byref targetTypeInfo As TypeInfo
                                                        ) As Boolean
       Dim parentBinaryExpression = TryCast(parentOfLiteral, BinaryExpressionSyntax)
       If parentBinaryExpression Is Nothing Then Return false
       If parentBinaryExpression.Left.Equals(literal) Then
          targetTypeInfo = sm.GetTypeInfo(parentBinaryExpression.Right, ct)
       Elseif parentBinaryExpression.Right.Equals(literal) Then
          targetTypeInfo =  sm.GetTypeInfo(parentBinaryExpression.Left, ct)
       Else
         Return false
       End If 
    return true
  End Function

  Friend Shared Function TryGetTypeInfoFromParentAssignment(parentOfLiteral As SyntaxNode ,
                                                            sm as SemanticModel,
                                                            ct As CancellationToken,
                                                      byref targetTypeInfo As TypeInfo) As Boolean
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
                syntaxkind.LeftShiftAssignmentStatement,
                SyntaxKind.RightShiftAssignmentStatement
           Case Else
                Return False
      End Select
      targetTypeInfo = sm.GetTypeInfo(parentAssignment.Left, ct)
      Return True
    End Function
End Class
