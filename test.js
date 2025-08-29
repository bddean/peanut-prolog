import { tokenz } from "pl-library/native/strings";

const start = performance.now();
const ts = tokenz(`
  % comment1
  /* comment 2 */
  /* comment
    * 3 */
    aasdf asdaQWER0_90 Vaar _avar _0 V123
    'quot' "QUooooTE" . etc '' "" \`\`
    
    -!@#$!
     123 -0.3e-5 0xFF 0b0101
`)
console.log(performance.now() - start);

console.log(ts);
