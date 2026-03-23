open System

type Tree=
    | Empty
    | Node of Value: int * Left: Tree* Right: Tree
     
// Находит позицию для создания новой ветки
let rec dataInput value root =
    match root with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) as node ->
        if value < v then 
            Node(v, dataInput value left, right)
        elif value > v then 
            Node(v, left, dataInput value right)
        else 
            node

// Выводит дерево в консоль
let rec printTree indent tree =
    match tree with
    | Empty -> printfn "%s*" indent
    | Node(value, left, right) ->
        printTree (indent + "     ") right
        printfn "%s%d" indent value
        printTree (indent + "     ") left

// Считывает целое число из консоли
let rec readInt prompt =
    printf "%s" prompt
    match Console.ReadLine() |> Int32.TryParse with
    | true, num when num > 0 -> num
    | _->
        printfn "Ошибка: введите целое положительное число!\n"
        readInt prompt

// Запрашивает число n раз и вызывает функцию вставки числа
let rec readNumbersForTree currentTree remaining =
    match remaining  with
    | 0 -> currentTree
    | _ ->
        let value = readInt "Введите число: "
        let newTree = dataInput value currentTree
        readNumbersForTree newTree (remaining  - 1)


// Удаляет старший разряд числа
let removeLeadingDigit number=
    if number < 10 then 
        0
    else
        let str = number.ToString()
        let newStr = str.Substring(1)
        int newStr

// Преобразует дерево, удаляя старший разряд из каждого значения
let rec transformTree tree=
    match tree with
    | Empty -> Empty
    | Node(value, left, right) ->
        let newValue = removeLeadingDigit value
        Node(newValue, transformTree left, transformTree right)


[<EntryPoint>]
let main _ =
    printfn "Программа удаления старшего разряда в каждом числе"
    printfn "==================================================="
    
    let count = readInt "Сколько чисел хотите ввести?\n"

    if count <= 0 then
        printfn "Дерево пусто"
    else
        let originalTree = readNumbersForTree Empty count
        printfn "\n\n============Исходное дерево============\n\n"
        printTree "" originalTree
        
        let transformedTree = transformTree originalTree
        printfn "\n\n==Дерево после удаления старшего разряда =="
        printTree "" transformedTree
    
    
    
    
    0
