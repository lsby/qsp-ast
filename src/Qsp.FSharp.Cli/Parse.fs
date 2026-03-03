module Qsp.FSharp.Cli.Parse
open Argu
open Qsp
open Qsp.Ast
open Qsp.Parser.Ast
open FParsec

open Qsp.FSharp.Cli.Commons

module AstToJson =
    open System.Text

    let private escapeJsonString (s: string) =
        let sb = StringBuilder(s.Length)
        for c in s do
            match c with
            | '"'  -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | c when System.Char.IsControl(c) ->
                sb.Append(sprintf "\\u%04x" (int c)) |> ignore
            | _ -> sb.Append(c) |> ignore
        sb.ToString()

    let private jsonStr (s: string) = sprintf "\"%s\"" (escapeJsonString s)
    let private jsonBool (b: bool) = if b then "true" else "false"
    let private jsonInt (i: int) = string i
    let private jsonNull = "null"

    let private jsonArr (items: string list) =
        sprintf "[%s]" (System.String.Join(",", items))

    let private jsonObj (pairs: (string * string) list) =
        pairs
        |> List.map (fun (k, v) -> sprintf "\"%s\":%s" k v)
        |> fun xs -> sprintf "{%s}" (System.String.Join(",", xs))

    let opToJson (op: Op) =
        jsonStr (Op.toString op)

    let unarOpToJson (op: UnarOp) =
        jsonStr (UnarOp.toString op)

    let varTypeToJson (vt: VarType) =
        match vt with
        | NumericType -> jsonStr "NumericType"
        | StringType -> jsonStr "StringType"

    let varToJson ((vt, name): Var) =
        jsonObj [
            "varType", varTypeToJson vt
            "name", jsonStr name
        ]

    let predefFuncToJson (f: Defines.PredefFunc) =
        jsonStr (sprintf "%A" f)

    let predefUndefToJson (pu: Defines.PredefFunc PredefUndef) =
        match pu with
        | Predef f ->
            jsonObj [ "type", jsonStr "Predef"; "value", predefFuncToJson f ]
        | Undef s ->
            jsonObj [ "type", jsonStr "Undef"; "value", jsonStr s ]

    let posToJson (pos: NoEqualityPosition) =
        jsonObj [
            "line", string pos.Pos.Line
            "column", string pos.Pos.Column
            "length", string pos.Pos.Length
        ]

    let rec stmtsOrRawToJson (x: StmtsOrRaw) =
        match x with
        | Raw s ->
            jsonObj [ "type", jsonStr "Raw"; "value", jsonStr s ]
        | StaticStmts stmts ->
            jsonObj [ "type", jsonStr "StaticStmts"; "body", posStatementsToJson stmts ]

    and posLineKindToJson ((pos, kind): PosLineKind) =
        let posObj =
            jsonObj [
                "line", string pos.Pos.Line
                "column", string pos.Pos.Column
                "length", string pos.Pos.Length
            ]
        match kind with
        | StringKind s ->
            jsonObj [ "pos", posObj; "type", jsonStr "StringKind"; "value", jsonStr s ]
        | ExprKind expr ->
            jsonObj [ "pos", posObj; "type", jsonStr "ExprKind"; "expr", exprToJson expr ]
        | HyperLinkKind(stmtsOrRaw, lines) ->
            jsonObj [
                "pos", posObj
                "type", jsonStr "HyperLinkKind"
                "stmts", stmtsOrRawToJson stmtsOrRaw
                "lines", jsonArr (lines |> List.map lineToJson)
            ]

    and lineToJson (line: Line) =
        jsonArr (line |> List.map posLineKindToJson)

    and valueToJson (v: Value) =
        match v with
        | Int i ->
            jsonObj [ "type", jsonStr "Int"; "value", jsonInt i ]
        | String lines ->
            jsonObj [ "type", jsonStr "String"; "lines", jsonArr (lines |> List.map lineToJson) ]

    and exprToJson (e: Expr) =
        match e with
        | Val v ->
            jsonObj [ "type", jsonStr "Val"; "value", valueToJson v ]
        | Expr.Var(var) ->
            jsonObj [ "type", jsonStr "Var"; "var", varToJson var ]
        | Func(predefUndef, args) ->
            jsonObj [
                "type", jsonStr "Func"
                "name", predefUndefToJson predefUndef
                "args", jsonArr (args |> List.map exprToJson)
            ]
        | Arr((pos, var), indices) ->
            jsonObj [
                "pos", posToJson pos
                "type", jsonStr "Arr"
                "var", varToJson var
                "indices", jsonArr (indices |> List.map exprToJson)
            ]
        | UnarExpr(op, expr) ->
            jsonObj [
                "type", jsonStr "UnarExpr"
                "op", unarOpToJson op
                "expr", exprToJson expr
            ]
        | Expr.Expr(op, left, right) ->
            jsonObj [
                "type", jsonStr "BinExpr"
                "op", opToJson op
                "left", exprToJson left
                "right", exprToJson right
            ]
        | Tuple exprs ->
            jsonObj [ "type", jsonStr "Tuple"; "items", jsonArr (exprs |> List.map exprToJson) ]

    and assignWhatToJson (aw: AssignWhat) =
        match aw with
        | AssignVar(var) ->
            jsonObj [ "type", jsonStr "AssignVar"; "var", varToJson var ]
        | AssignArr((pos, var), args) ->
            jsonObj [
                "pos", posToJson pos
                "type", jsonStr "AssignArr"
                "var", varToJson var
                "indices", jsonArr (args |> List.map exprToJson)
            ]

    and statementToJson (stmt: Statement) =
        match stmt with
        | Assign(isLocal, targets, expr) ->
            jsonObj [
                "type", jsonStr "Assign"
                "isLocal", jsonBool isLocal
                "targets", jsonArr (targets |> List.map assignWhatToJson)
                "value", exprToJson expr
            ]
        | AssignCode(target, body) ->
            jsonObj [
                "type", jsonStr "AssignCode"
                "target", assignWhatToJson target
                "body", posStatementsToJson body
            ]
        | Proc(name, args) ->
            jsonObj [
                "type", jsonStr "Proc"
                "name", jsonStr name
                "args", jsonArr (args |> List.map exprToJson)
            ]
        | If(cond, thenBody, elseBody) ->
            jsonObj [
                "type", jsonStr "If"
                "cond", exprToJson cond
                "then", posStatementsToJson thenBody
                "else", posStatementsToJson elseBody
            ]
        | Act(exprs, body) ->
            jsonObj [
                "type", jsonStr "Act"
                "exprs", jsonArr (exprs |> List.map exprToJson)
                "body", posStatementsToJson body
            ]
        | For(var, from, to', step, body) ->
            jsonObj [
                "type", jsonStr "For"
                "var", varToJson var
                "from", exprToJson from
                "to", exprToJson to'
                "step", (match step with Some s -> exprToJson s | None -> jsonNull)
                "body", posStatementsToJson body
            ]
        | Loop(preStmts, cond, step, body) ->
            jsonObj [
                "type", jsonStr "Loop"
                "preStmts", posStatementsToJson preStmts
                "cond", exprToJson cond
                "step", posStatementsToJson step
                "body", posStatementsToJson body
            ]
        | Label name ->
            jsonObj [ "type", jsonStr "Label"; "name", jsonStr name ]
        | Comment text ->
            jsonObj [ "type", jsonStr "Comment"; "text", jsonStr text ]
        | Exit ->
            jsonObj [ "type", jsonStr "Exit" ]

    and posStatementToJson ((pos, stmt): PosStatement) =
        jsonObj [
            "pos", jsonObj [
                "line", string pos.Pos.Line
                "column", string pos.Pos.Column
                "length", string pos.Pos.Length
            ]
            "stmt", statementToJson stmt
        ]

    and posStatementsToJson (stmts: PosStatement list) =
        jsonArr (stmts |> List.map posStatementToJson)

    let locationToJson (Location(name, body)) =
        jsonObj [
            "type", jsonStr "Location"
            "name", jsonStr name
            "body", posStatementsToJson body
        ]

    let documentElementToJson (elem: DocumentElement) =
        match elem with
        | DocumentElement.Location loc ->
            locationToJson loc
        | DocumentElement.CommentLine text ->
            jsonObj [ "type", jsonStr "CommentLine"; "text", jsonStr text ]

    let documentToJson (doc: Document) =
        jsonArr (doc |> List.map documentElementToJson)

[<RequireQualifiedAccess>]
type ParseCliArguments =
    | [<MainCommand; ExactlyOnce>] Source_Path of path: FilePath
    | [<AltCommandLine("-o")>] Output of path: FilePath
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Source_Path _ -> "path to source game file (.qsps or .txt)."
            | Output _ -> "output file path. If not specified, prints to stdout."

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ParseCliArguments =
    let exec (results: ParseResults<ParseCliArguments>) =
        let sourcePath = results.GetResult ParseCliArguments.Source_Path
        let outputPath = results.TryGetResult ParseCliArguments.Output
        let enc = System.Text.Encoding.UTF8

        if not (System.IO.File.Exists sourcePath) then
            eprintfn "Error: file not found: %s" sourcePath
        else

        let content = System.IO.File.ReadAllText(sourcePath, enc)
        let result = Document.start content

        match result with
        | Success(doc, _state, _) ->
            let json = AstToJson.documentToJson doc
            match outputPath with
            | Some path ->
                System.IO.File.WriteAllText(path, json, enc)
                eprintfn "AST written to: %s" path
            | None ->
                printfn "%s" json

        | Failure(errMsg, _err, _state) ->
            eprintfn "Parse error:\n%s" errMsg
