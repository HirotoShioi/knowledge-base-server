# Question
まれに101文字のアドレスを生成され、それによってAdaを送信ができなくなる
#### Locale
ja
## Answer
現在のウォレットには稀に正常なアドレスが生成されない不具合があります。このアドレスでは手数料を正しく計算することができない為、送金ができません。
またこのアドレスを用いて送金を試みると、「手数料を支払うためのADAが不足しています。より少額の送金を試みてください。」というエラーメッセージが表示されます。
これらのアドレスの特徴としては通常の104文字のアドレスよりも3文字短い、101文字のアドレスであることが挙げられます。
回避策としては、もし送金時のアドレスが101文字である場合には受信者に新しいアドレスを要求してください。
また取引所からの引き出す場合には、ダイダロスを用いて新しいアドレスを生成し、それを使用してください。