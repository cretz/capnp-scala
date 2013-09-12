Cap'n Proto Scala

Beginning of implementation, nothing to see here yet...

Caveats:

* Writing primitive fields updates the internal byte buffer, but writing sequences, text fields, unions, etc does NOT update the internal byte buffer
* Setting fields no an inner group/union will immediately update byte buffer whether or not it's set on the union field