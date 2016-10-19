object Test01 extends App {
val mySay = new SayWord();
val result = mySay.say("Okan"); 
if (result == "Hello Okan"){
println("SUCCESS");
 } else {
println("FAILED "); 
}
}