// For more information see https://aka.ms/fsharp-console-apps

// Test1_Pratik Shrestha_23026137

let empsalaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let calculateTax salary =
    match salary with
    | salary when salary <= 49020 -> float salary * 0.15
    | salary when salary <= 98040 -> float salary * 0.205
    | salary when salary <= 151978 -> float salary * 0.26
    | salary when salary <= 216511 -> float salary * 0.29
    | salary -> float salary * 0.33

let highIncomeSalaries = empsalaries |> List.filter (fun salary -> salary > 100000)
printfn "Actual Salaries: %A" empsalaries
printfn "High-income salaries are: %A" highIncomeSalaries
printfn ""

let taxes = empsalaries |> List.map calculateTax
printfn "Actual salaries: %A" empsalaries
printfn "Taxes for listed salaries are: %A" taxes
printfn ""

let addedSalaries = empsalaries |> List.map (fun salary -> if salary < 49020 then salary + 20000 else salary)
printfn "Actual salaries: %A" empsalaries
printfn "UPdated low salaries: %A" addedSalaries
printfn ""

let sumedSalaries = 
    empsalaries
    |> List.filter (fun salary -> salary >= 50000 && salary <= 100000)
    |> List.fold (+) 0
printfn "Actual salaries: %A" empsalaries
printfn "Sum of salaries between $50,000 and $100,000 are: %d" sumedSalaries
printfn ""


//For Tail recursion

let summultiple limit =
    let rec calculateSum currentMultiple accumulator =
        if currentMultiple > limit then
            accumulator
        else
            calculateSum (currentMultiple + 3) (accumulator + currentMultiple)
    
    calculateSum 3 0

let output = summultiple 27
printfn "Sum of multiples of 3 up to 27: %d" output
