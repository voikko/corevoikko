# TypeScript typings for JavaScript version of libvoikko

See wiki
[Building libvoikko for JavaScript](https://github.com/voikko/corevoikko/wiki/JavaScript)
to compile JavaScript version of libvoikko and voikko-fi.

## Example

Create `main.ts`:

```typescript
import Libvoikko from './libvoikko';

const voikko = new Libvoikko();
const v = voikko.init('fi');

const a = v.analyze('alustaa');
console.log(a);
```

Transpile TypeScript to JavaScript and run with Node.js

```bash
npx -p typescript tsc --esModuleInterop --target es5 main.ts
node main.js
```
