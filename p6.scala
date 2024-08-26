@main def p6():Unit={
    println("****************Q1******************")
    println(productNames(inventory1))
    println(totalValue(inventory1))
    println(emptyInventory(inventory1))
    println(mergeInventories(inventory1,inventory2))

    println("****************Q2******************")
    getStudentInfo()


}

case class Product(name:String,price:Double,quantity:Int);

val inventory1:Map[Int,Product]=Map(
    100->Product("apple",100.0,10),
    101->Product("orange",200.5,20),
    102->Product("banana",350.6,30)

);

val inventory2:Map[Int,Product]=Map(
    100->Product("apple",150.0,30),
    101->Product("orange",265.5,5),
    103->Product("woodapple",400.0,20)

);

def productNames(inventory:Map[Int,Product]): List[String]={
    inventory.values.map(product => product.name).toList
}

def totalValue(inventory:Map[Int,Product]):Double={
    inventory.values.map(product=>product.price * product.quantity).sum
}
def emptyInventory(inventory:Map[Int,Product]):Boolean={
    inventory.values.map(product => product.quantity).sum == 0
}

def mergeInventories(inventory1:Map[Int,Product],inventory2:Map[Int,Product]):Map[Int,Product]={

    var mergedInventory= inventory1;

    inventory2.foreach((id,product) => {
        if(mergedInventory.contains(id))  {
            val name = mergedInventory(id).name;
            val price = Math.max(mergedInventory(id).price,product.price);
            val quantity = mergedInventory(id).quantity + product.quantity;
            mergedInventory=mergedInventory.updated(id,Product(name,price,quantity));

        }
        else{
            mergedInventory=mergedInventory.updated(id,product);
        }
    });

    mergedInventory;
}

def findProduct(id: Int, inventory: Map[Int, Product]): Unit = {
  if (inventory.contains(id)) { 
    val product = inventory(id)
    println(s"ID $id found:")
    println(s"\tName: ${product.name}")
    println(s"\tPrice: ${product.price}")
    println(s"\tQuantity: ${product.quantity}")
  } else {
    println(s"ID $id not found.")
  }
}

def getStudentInfo() = {
    val student = getStudentInfoWithRetry();

    val percentage = student(1) * 100.0 / student(2);
    val grade = if percentage >= 90.0 then 
                    'A'
                else if percentage >= 75.0 then 
                    'B' 
                else if percentage >= 50.0 then 
                    'C' 
                else 'D';

    printRecords(student ++ (percentage, grade));
}

def printRecords(student:(String,Int,Int,Double,Char))={
    println("Name: " + student(0));
    println("Marks: " + student(1));
    println("Total Possible Marks: " + student(2));
    println("Percentage: " + student(3) + "%");
    println("Grade: " + student(4));

}
def validateInput(student:(String,Int,Int)):(Boolean,Option[String])={
    if(student(0).trim == ""){
        (false,Some("Name cannot be empty!"))

    }
    else if(student(1) < 0 || student(1) > student(2)){
        (false,Some("Marks must be between 0 and total possible marks."))
    }
    else{
        (true,None)
    }
}

def getStudentInfoWithRetry():(String,Int,Int)={
    print("Enter student name: ");
    val name = scala.io.StdIn.readLine();
    print("Enter student marks: ");
    val marks = scala.io.StdIn.readInt();
    print("Enter total marks: ");
    val totalMarks = scala.io.StdIn.readInt();

    val (isValid,errorMessage)=validateInput((name,marks,totalMarks));
    if (isValid){
        (name,marks,totalMarks)
    }
    else{
        println(errorMessage.get);
        getStudentInfoWithRetry();
    }
        

}
