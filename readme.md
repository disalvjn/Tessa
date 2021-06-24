# Tessa!
Tessa is a minimalist stack-based programming language that creates tessellations (repeating geometric patterns).

See the Examples directory: 
- [roses.tessa](Examples/roses.tessa) is the script that produces [roses.png](Examples/roses.png)
- [ocean-bottom.tessa](Examples/ocean-bottom.tessa) is the script that produces [ocean-bottom.png](Examples/ocean-bottom.png)

The Tessa interpreter is written in F# with the Fable compiler (F# -> JS). An interactive interpreter runs in the browser.
Example of use: [demo-roses-4.png](Examples/demo-roses-4.png). Also see the rest of that series for an example of
how the tessellation is built up iteratively throughout the script.

To learn more about tessellations, check out *Designing Tessellations : The Secrets of Interlocking Patterns* by Jinny Beyer.

# How to run
In Tessa/, `npm run start`. Navigate to localhost:8080. In the top right frame, write your script and press the '`' key to evaluate it, 
which will display the result on the left.