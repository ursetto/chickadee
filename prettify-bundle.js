var p=null;window.PR_SHOULD_USE_CONTINUATION=!0;
(function(){function L(a){function l(a){var h=a.charCodeAt(0);if(h!==92)return h;var b=a.charAt(1);return(h=q[b])?h:"0"<=b&&b<="7"?parseInt(a.substring(1),8):b==="u"||b==="x"?parseInt(a.substring(2),16):a.charCodeAt(1)}function e(a){if(a<32)return(a<16?"\\x0":"\\x")+a.toString(16);a=String.fromCharCode(a);return a==="\\"||a==="-"||a==="]"||a==="^"?"\\"+a:a}function i(a){var h=a.substring(1,a.length-1).match(/\\u[\dA-Fa-f]{4}|\\x[\dA-Fa-f]{2}|\\[0-3][0-7]{0,2}|\\[0-7]{1,2}|\\[\S\s]|[^\\]/g),a=[],b=
h[0]==="^",m=["["];b&&m.push("^");for(var b=b?1:0,d=h.length;b<d;++b){var g=h[b];if(/\\[bdsw]/i.test(g))m.push(g);else{var g=l(g),f;b+2<d&&"-"===h[b+1]?(f=l(h[b+2]),b+=2):f=g;a.push([g,f]);f<65||g>122||(f<65||g>90||a.push([Math.max(65,g)|32,Math.min(f,90)|32]),f<97||g>122||a.push([Math.max(97,g)&-33,Math.min(f,122)&-33]))}}a.sort(function(a,b){return a[0]-b[0]||b[1]-a[1]});h=[];d=[];for(b=0;b<a.length;++b)g=a[b],g[0]<=d[1]+1?d[1]=Math.max(d[1],g[1]):h.push(d=g);for(b=0;b<h.length;++b)g=h[b],m.push(e(g[0])),
g[1]>g[0]&&(g[1]+1>g[0]&&m.push("-"),m.push(e(g[1])));m.push("]");return m.join("")}function y(a){for(var h=a.source.match(/\[(?:[^\\\]]|\\[\S\s])*]|\\u[\dA-Fa-f]{4}|\\x[\dA-Fa-f]{2}|\\\d+|\\[^\dux]|\(\?[!:=]|[()^]|[^()[\\^]+/g),b=h.length,f=[],d=0,g=0;d<b;++d){var c=h[d];c==="("?++g:"\\"===c.charAt(0)&&(c=+c.substring(1))&&(c<=g?f[c]=-1:h[d]=e(c))}for(d=1;d<f.length;++d)-1===f[d]&&(f[d]=++s);for(g=d=0;d<b;++d)c=h[d],c==="("?(++g,f[g]||(h[d]="(?:")):"\\"===c.charAt(0)&&(c=+c.substring(1))&&c<=g&&
(h[d]="\\"+f[g]);for(g=d=0;d<b;++d)"^"===h[d]&&"^"!==h[d+1]&&(h[d]="");if(a.ignoreCase&&r)for(d=0;d<b;++d)c=h[d],a=c.charAt(0),c.length>=2&&a==="["?h[d]=i(c):a!=="\\"&&(h[d]=c.replace(/[A-Za-z]/g,function(a){a=a.charCodeAt(0);return"["+String.fromCharCode(a&-33,a|32)+"]"}));return h.join("")}for(var s=0,r=!1,k=!1,o=0,f=a.length;o<f;++o){var c=a[o];if(c.ignoreCase)k=!0;else if(/[a-z]/i.test(c.source.replace(/\\u[\da-f]{4}|\\x[\da-f]{2}|\\[^UXux]/gi,""))){r=!0;k=!1;break}}for(var q={b:8,t:9,n:10,v:11,
f:12,r:13},n=[],o=0,f=a.length;o<f;++o){c=a[o];if(c.global||c.multiline)throw Error(""+c);n.push("(?:"+y(c)+")")}return RegExp(n.join("|"),k?"gi":"g")}function M(a){function l(a){switch(a.nodeType){case 1:if(e.test(a.className))break;for(var c=a.firstChild;c;c=c.nextSibling)l(c);c=a.nodeName;if("BR"===c||"LI"===c)i[r]="\n",s[r<<1]=y++,s[r++<<1|1]=a;break;case 3:case 4:c=a.nodeValue,c.length&&(c=o?c.replace(/\r\n?/g,"\n"):c.replace(/[\t\n\r ]+/g," "),i[r]=c,s[r<<1]=y,y+=c.length,s[r++<<1|1]=a)}}var e=
/(?:^|\s)nocode(?:\s|$)/,i=[],y=0,s=[],r=0,k;a.currentStyle?k=a.currentStyle.whiteSpace:window.getComputedStyle&&(k=document.defaultView.getComputedStyle(a,p).getPropertyValue("white-space"));var o=k&&"pre"===k.substring(0,3);l(a);return{a:i.join("").replace(/\n$/,""),c:s}}function B(a,l,e,i){l&&(a={a:l,d:a},e(a),i.push.apply(i,a.e))}function u(a,l){function e(a){for(var k=a.d,o=[k,"pln"],f=0,c=a.a.match(y)||[],q={},n=0,z=c.length;n<z;++n){var h=c[n],b=q[h],m=void 0,d;if(typeof b==="string")d=!1;
else{var g=i[h.charAt(0)];if(g)m=h.match(g[1]),b=g[0];else{for(d=0;d<s;++d)if(g=l[d],m=h.match(g[1])){b=g[0];break}m||(b="pln")}if((d=b.length>=5&&"lang-"===b.substring(0,5))&&!(m&&typeof m[1]==="string"))d=!1,b="src";d||(q[h]=b)}g=f;f+=h.length;if(d){d=m[1];var j=h.indexOf(d),A=j+d.length;m[2]&&(A=h.length-m[2].length,j=A-d.length);b=b.substring(5);B(k+g,h.substring(0,j),e,o);B(k+g+j,d,C(b,d),o);B(k+g+A,h.substring(A),e,o)}else o.push(k+g,b)}a.e=o}var i={},y;(function(){for(var e=a.concat(l),k=[],
o={},f=0,c=e.length;f<c;++f){var q=e[f],n=q[3];if(n)for(var j=n.length;--j>=0;)i[n.charAt(j)]=q;q=q[1];n=""+q;o.hasOwnProperty(n)||(k.push(q),o[n]=p)}k.push(/[\S\s]/);y=L(k)})();var s=l.length;return e}function t(a){var l=[],e=[];a.tripleQuotedStrings?l.push(["str",/^(?:'''(?:[^'\\]|\\[\S\s]|''?(?=[^']))*(?:'''|$)|"""(?:[^"\\]|\\[\S\s]|""?(?=[^"]))*(?:"""|$)|'(?:[^'\\]|\\[\S\s])*(?:'|$)|"(?:[^"\\]|\\[\S\s])*(?:"|$))/,p,"'\""]):a.multiLineStrings?l.push(["str",/^(?:'(?:[^'\\]|\\[\S\s])*(?:'|$)|"(?:[^"\\]|\\[\S\s])*(?:"|$)|`(?:[^\\`]|\\[\S\s])*(?:`|$))/,
p,"'\"`"]):l.push(["str",/^(?:'(?:[^\n\r'\\]|\\.)*(?:'|$)|"(?:[^\n\r"\\]|\\.)*(?:"|$))/,p,"\"'"]);a.verbatimStrings&&e.push(["str",/^@"(?:[^"]|"")*(?:"|$)/,p]);var i=a.hashComments;i&&(a.cStyleComments?(i>1?l.push(["com",/^#(?:##(?:[^#]|#(?!##))*(?:###|$)|.*)/,p,"#"]):l.push(["com",/^#(?:(?:define|elif|else|endif|error|ifdef|include|ifndef|line|pragma|undef|warning)\b|[^\n\r]*)/,p,"#"]),e.push(["str",/^<(?:(?:(?:\.\.\/)*|\/?)(?:[\w-]+(?:\/[\w-]+)+)?[\w-]+\.h|[a-z]\w*)>/,p])):l.push(["com",/^#[^\n\r]*/,
p,"#"]));a.cStyleComments&&(e.push(["com",/^\/\/[^\n\r]*/,p]),e.push(["com",/^\/\*[\S\s]*?(?:\*\/|$)/,p]));a.regexLiterals&&e.push(["lang-regex",/^(?:^^\.?|[+-]|[!=]={0,2}|#|%=?|&&?=?|\(|\*=?|[+-]=|->|\/=?|::?|<<?=?|>{1,3}=?|[,;?@[{~]|\^\^?=?|\|\|?=?|break|case|continue|delete|do|else|finally|instanceof|return|throw|try|typeof)\s*(\/(?=[^*/])(?:[^/[\\]|\\[\S\s]|\[(?:[^\\\]]|\\[\S\s])*(?:]|$))+\/)/]);(i=a.types)&&e.push(["typ",i]);a=(""+a.keywords).replace(/^ | $/g,"");a.length&&e.push(["kwd",RegExp("^(?:"+
a.replace(/[\s,]+/g,"|")+")\\b"),p]);l.push(["pln",/^\s+/,p," \r\n\t\xa0"]);e.push(["lit",/^@[$_a-z][\w$@]*/i,p],["typ",/^(?:[@_]?[A-Z]+[a-z][\w$@]*|\w+_t\b)/,p],["pln",/^[$_a-z][\w$@]*/i,p],["lit",/^(?:0x[\da-f]+|(?:\d(?:_\d+)*\d*(?:\.\d*)?|\.\d\+)(?:e[+-]?\d+)?)[a-z]*/i,p,"0123456789"],["pln",/^\\[\S\s]?/,p],["pun",/^.[^\s\w"-$'./@\\`]*/,p]);return u(l,e)}function D(a,l){function e(a){switch(a.nodeType){case 1:if(j.test(a.className))break;if("BR"===a.nodeName)i(a),a.parentNode&&a.parentNode.removeChild(a);
else for(a=a.firstChild;a;a=a.nextSibling)e(a);break;case 3:case 4:if(o){var b=a.nodeValue,c=b.match(s);if(c){var d=b.substring(0,c.index);a.nodeValue=d;(b=b.substring(c.index+c[0].length))&&a.parentNode.insertBefore(r.createTextNode(b),a.nextSibling);i(a);d||a.parentNode.removeChild(a)}}}}function i(a){function b(a,c){var f=c?a.cloneNode(!1):a,e=a.parentNode;if(e){var e=b(e,1),h=a.nextSibling;e.appendChild(f);for(var i=h;i;i=h)h=i.nextSibling,e.appendChild(i)}return f}for(;!a.nextSibling;)if(a=a.parentNode,
!a)return;for(var a=b(a.nextSibling,0),c;(c=a.parentNode)&&c.nodeType===1;)a=c;f.push(a)}var j=/(?:^|\s)nocode(?:\s|$)/,s=/\r\n?|\n/,r=a.ownerDocument,k;a.currentStyle?k=a.currentStyle.whiteSpace:window.getComputedStyle&&(k=r.defaultView.getComputedStyle(a,p).getPropertyValue("white-space"));var o=k&&"pre"===k.substring(0,3);for(k=r.createElement("LI");a.firstChild;)k.appendChild(a.firstChild);for(var f=[k],c=0;c<f.length;++c)e(f[c]);l===(l|0)&&f[0].setAttribute("value",l);var q=r.createElement("OL");
q.className="linenums";for(var n=Math.max(0,l-1|0)||0,c=0,z=f.length;c<z;++c)k=f[c],k.className="L"+(c+n)%10,k.firstChild||k.appendChild(r.createTextNode("\xa0")),q.appendChild(k);a.appendChild(q)}function j(a,l){for(var e=l.length;--e>=0;){var i=l[e];w.hasOwnProperty(i)?window.console&&console.warn("cannot override language handler %s",i):w[i]=a}}function C(a,l){if(!a||!w.hasOwnProperty(a))a=/^\s*</.test(l)?"default-markup":"default-code";return w[a]}function E(a){var l=a.g;try{var e=M(a.h),i=e.a;a.a=
i;a.c=e.c;a.d=0;C(l,i)(a);var j=/\bMSIE\b/.test(navigator.userAgent),l=/\n/g,s=a.a,r=s.length,e=0,k=a.c,o=k.length,i=0,f=a.e,c=f.length,a=0;f[c]=r;var q,n;for(n=q=0;n<c;)f[n]!==f[n+2]?(f[q++]=f[n++],f[q++]=f[n++]):n+=2;c=q;for(n=q=0;n<c;){for(var z=f[n],h=f[n+1],b=n+2;b+2<=c&&f[b+1]===h;)b+=2;f[q++]=z;f[q++]=h;n=b}for(f.length=q;i<o;){var m=k[i+2]||r,d=f[a+2]||r,b=Math.min(m,d),g=k[i+1],t;if(g.nodeType!==1&&(t=s.substring(e,b))){j&&(t=t.replace(l,"\r"));g.nodeValue=t;var v=g.ownerDocument,u=v.createElement("SPAN");
u.className=f[a+1];var x=g.parentNode;x.replaceChild(u,g);u.appendChild(g);e<m&&(k[i+1]=g=v.createTextNode(s.substring(b,m)),x.insertBefore(g,u.nextSibling))}e=b;e>=m&&(i+=2);e>=d&&(a+=2)}}catch(w){"console"in window&&console.log(w&&w.stack?w.stack:w)}}var v=["break,continue,do,else,for,if,return,while"],x=[[v,"auto,case,char,const,default,double,enum,extern,float,goto,int,long,register,short,signed,sizeof,static,struct,switch,typedef,union,unsigned,void,volatile"],"catch,class,delete,false,import,new,operator,private,protected,public,this,throw,true,try,typeof"],
F=[x,"alignof,align_union,asm,axiom,bool,concept,concept_map,const_cast,constexpr,decltype,dynamic_cast,explicit,export,friend,inline,late_check,mutable,namespace,nullptr,reinterpret_cast,static_assert,static_cast,template,typeid,typename,using,virtual,where"],G=[x,"abstract,boolean,byte,extends,final,finally,implements,import,instanceof,null,native,package,strictfp,super,synchronized,throws,transient"],H=[G,"as,base,by,checked,decimal,delegate,descending,dynamic,event,fixed,foreach,from,group,implicit,in,interface,internal,into,is,lock,object,out,override,orderby,params,partial,readonly,ref,sbyte,sealed,stackalloc,string,select,uint,ulong,unchecked,unsafe,ushort,var"],
x=[x,"debugger,eval,export,function,get,null,set,undefined,var,with,Infinity,NaN"],I=[v,"and,as,assert,class,def,del,elif,except,exec,finally,from,global,import,in,is,lambda,nonlocal,not,or,pass,print,raise,try,with,yield,False,True,None"],J=[v,"alias,and,begin,case,class,def,defined,elsif,end,ensure,false,in,module,next,nil,not,or,redo,rescue,retry,self,super,then,true,undef,unless,until,when,yield,BEGIN,END"],v=[v,"case,done,elif,esac,eval,fi,function,in,local,set,then,until"],K=/^(DIR|FILE|vector|(de|priority_)?queue|list|stack|(const_)?iterator|(multi)?(set|map)|bitset|u?(int|float)\d*)/,
N=/\S/,O=t({keywords:[F,H,x,"caller,delete,die,do,dump,elsif,eval,exit,foreach,for,goto,if,import,last,local,my,next,no,our,print,package,redo,require,sub,undef,unless,until,use,wantarray,while,BEGIN,END"+I,J,v],hashComments:!0,cStyleComments:!0,multiLineStrings:!0,regexLiterals:!0}),w={};j(O,["default-code"]);j(u([],[["pln",/^[^<?]+/],["dec",/^<!\w[^>]*(?:>|$)/],["com",/^<\!--[\S\s]*?(?:--\>|$)/],["lang-",/^<\?([\S\s]+?)(?:\?>|$)/],["lang-",/^<%([\S\s]+?)(?:%>|$)/],["pun",/^(?:<[%?]|[%?]>)/],["lang-",
/^<xmp\b[^>]*>([\S\s]+?)<\/xmp\b[^>]*>/i],["lang-js",/^<script\b[^>]*>([\S\s]*?)(<\/script\b[^>]*>)/i],["lang-css",/^<style\b[^>]*>([\S\s]*?)(<\/style\b[^>]*>)/i],["lang-in.tag",/^(<\/?[a-z][^<>]*>)/i]]),["default-markup","htm","html","mxml","xhtml","xml","xsl"]);j(u([["pln",/^\s+/,p," \t\r\n"],["atv",/^(?:"[^"]*"?|'[^']*'?)/,p,"\"'"]],[["tag",/^^<\/?[a-z](?:[\w-.:]*\w)?|\/?>$/i],["atn",/^(?!style[\s=]|on)[a-z](?:[\w:-]*\w)?/i],["lang-uq.val",/^=\s*([^\s"'>]*(?:[^\s"'/>]|\/(?=\s)))/],["pun",/^[/<->]+/],
["lang-js",/^on\w+\s*=\s*"([^"]+)"/i],["lang-js",/^on\w+\s*=\s*'([^']+)'/i],["lang-js",/^on\w+\s*=\s*([^\s"'>]+)/i],["lang-css",/^style\s*=\s*"([^"]+)"/i],["lang-css",/^style\s*=\s*'([^']+)'/i],["lang-css",/^style\s*=\s*([^\s"'>]+)/i]]),["in.tag"]);j(u([],[["atv",/^[\S\s]+/]]),["uq.val"]);j(t({keywords:F,hashComments:!0,cStyleComments:!0,types:K}),["c","cc","cpp","cxx","cyc","m"]);j(t({keywords:"null,true,false"}),["json"]);j(t({keywords:H,hashComments:!0,cStyleComments:!0,verbatimStrings:!0,types:K}),
["cs"]);j(t({keywords:G,cStyleComments:!0}),["java"]);j(t({keywords:v,hashComments:!0,multiLineStrings:!0}),["bsh","csh","sh"]);j(t({keywords:I,hashComments:!0,multiLineStrings:!0,tripleQuotedStrings:!0}),["cv","py"]);j(t({keywords:"caller,delete,die,do,dump,elsif,eval,exit,foreach,for,goto,if,import,last,local,my,next,no,our,print,package,redo,require,sub,undef,unless,until,use,wantarray,while,BEGIN,END",hashComments:!0,multiLineStrings:!0,regexLiterals:!0}),["perl","pl","pm"]);j(t({keywords:J,hashComments:!0,
multiLineStrings:!0,regexLiterals:!0}),["rb"]);j(t({keywords:x,cStyleComments:!0,regexLiterals:!0}),["js"]);j(t({keywords:"all,and,by,catch,class,else,extends,false,finally,for,if,in,is,isnt,loop,new,no,not,null,of,off,on,or,return,super,then,true,try,unless,until,when,while,yes",hashComments:3,cStyleComments:!0,multilineStrings:!0,tripleQuotedStrings:!0,regexLiterals:!0}),["coffee"]);j(u([],[["str",/^[\S\s]+/]]),["regex"]);window.prettyPrintOne=function(a,l,e){var i=document.createElement("PRE");
i.innerHTML=a;e&&D(i,e);E({g:l,i:e,h:i});return i.innerHTML};window.prettyPrint=function(a){function l(){for(var e=window.PR_SHOULD_USE_CONTINUATION?k.now()+250:Infinity;o<i.length&&k.now()<e;o++){var n=i[o],j=n.className;if(j.indexOf("prettyprint")>=0){var j=j.match(c),h,b;if(b=!j){b=n;for(var m=void 0,d=b.firstChild;d;d=d.nextSibling)var g=d.nodeType,m=g===1?m?b:d:g===3?N.test(d.nodeValue)?b:m:m;b=(h=m===b?void 0:m)&&"CODE"===h.tagName}b&&(j=h.className.match(c));j&&(j=j[1]);b=!1;for(m=n.parentNode;m;m=
m.parentNode)if((m.tagName==="pre"||m.tagName==="code"||m.tagName==="xmp")&&m.className&&m.className.indexOf("prettyprint")>=0){b=!0;break}b||((b=(b=n.className.match(/\blinenums\b(?::(\d+))?/))?b[1]&&b[1].length?+b[1]:!0:!1)&&D(n,b),f={g:j,h:n,i:b},E(f))}}o<i.length?setTimeout(l,250):a&&a()}for(var e=[document.getElementsByTagName("pre"),document.getElementsByTagName("code"),document.getElementsByTagName("xmp")],i=[],j=0;j<e.length;++j)for(var s=0,r=e[j].length;s<r;++s)i.push(e[j][s]);var e=p,k=
Date;k.now||(k={now:function(){return+new Date}});var o=0,f,c=/\blang(?:uage)?-([\w.]+)(?!\S)/;l()};window.PR={createSimpleLexer:u,registerLangHandler:j,sourceDecorator:t,PR_ATTRIB_NAME:"atn",PR_ATTRIB_VALUE:"atv",PR_COMMENT:"com",PR_DECLARATION:"dec",PR_KEYWORD:"kwd",PR_LITERAL:"lit",PR_NOCODE:"nocode",PR_PLAIN:"pln",PR_PUNCTUATION:"pun",PR_SOURCE:"src",PR_STRING:"str",PR_TAG:"tag",PR_TYPE:"typ"}})();
PR.registerLangHandler(PR.createSimpleLexer([["pln",/^[\t\n\f\r ]+/,null," \t\r\n"]],[["str",/^"(?:[^\n\f\r"\\]|\\(?:\r\n?|\n|\f)|\\[\S\s])*"/,null],["str",/^'(?:[^\n\f\r'\\]|\\(?:\r\n?|\n|\f)|\\[\S\s])*'/,null],["lang-css-str",/^url\(([^"')]*)\)/i],["kwd",/^(?:url|rgb|!important|@import|@page|@media|@charset|inherit)(?=[^\w-]|$)/i,null],["lang-css-kw",/^(-?(?:[_a-z]|\\[\da-f]+ ?)(?:[\w-]|\\\\[\da-f]+ ?)*)\s*:/i],["com",/^\/\*[^*]*\*+(?:[^*/][^*]*\*+)*\//],["com",
/^(?:<\!--|--\>)/],["lit",/^(?:\d+|\d*\.\d+)(?:%|[a-z]+)?/i],["lit",/^#[\da-f]{3,6}/i],["pln",/^-?(?:[_a-z]|\\[\da-f]+ ?)(?:[\w-]|\\\\[\da-f]+ ?)*/i],["pun",/^[^\s\w"']+/]]),["css"]);PR.registerLangHandler(PR.createSimpleLexer([],[["kwd",/^-?(?:[_a-z]|\\[\da-f]+ ?)(?:[\w-]|\\\\[\da-f]+ ?)*/i]]),["css-kw"]);PR.registerLangHandler(PR.createSimpleLexer([],[["str",/^[^"')]+/]]),["css-str"]);
var a=null;
PR.registerLangHandler(PR.createSimpleLexer([["opn",/^\(+/,a,"("],["clo",/^\)+/,a,")"],["com",/^;[^\n\r]*/,a,";"],["pln",/^[\t\n\r \xa0]+/,a,"\t\n\r \xa0"],["str",/^"(?:[^"\\]|\\[\S\s])*(?:"|$)/,a,'"']],[["kwd",/^(?:block|c[ad]+r|catch|con[ds]|def(?:ine|un)|do|eq|eql|equal|equalp|eval-when|flet|format|go|if|labels|lambda|let|load-time-value|locally|macrolet|multiple-value-call|nil|progn|progv|quote|require|return-from|setq|symbol-macrolet|t|tagbody|the|throw|unwind)\b/,a],
["lit",/^[+-]?(?:[#0]x[\da-f]+|\d+\/\d+|(?:\.\d+|\d+(?:\.\d*)?)(?:[de][+-]?\d+)?)/i],["lit",/^'(?:-*(?:\w|\\[!-~])(?:[\w-]*|\\[!-~])[!=?]?)?/],["pln",/^-*(?:[_a-z]|\\[!-~])(?:[\w-]*|\\[!-~])[!=?]?/i],["pun",/^[^\w\t\n\r "'-);\\\xa0]+/]]),["cl","el","lisp","lsp","scm"]);
PR.registerLangHandler(PR.createSimpleLexer([["pln",/^[\t\n\r \xa0]+/,null,"\t\n\r �\xa0"],["str",/^(?:"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*')/,null,"\"'"]],[["com",/^(?:--[^\n\r]*|\/\*[\S\s]*?(?:\*\/|$))/],["kwd",/^(?:add|all|alter|and|any|as|asc|authorization|backup|begin|between|break|browse|bulk|by|cascade|case|check|checkpoint|close|clustered|coalesce|collate|column|commit|compute|constraint|contains|containstable|continue|convert|create|cross|current|current_date|current_time|current_timestamp|current_user|cursor|database|dbcc|deallocate|declare|default|delete|deny|desc|disk|distinct|distributed|double|drop|dummy|dump|else|end|errlvl|escape|except|exec|execute|exists|exit|fetch|file|fillfactor|for|foreign|freetext|freetexttable|from|full|function|goto|grant|group|having|holdlock|identity|identitycol|identity_insert|if|in|index|inner|insert|intersect|into|is|join|key|kill|left|like|lineno|load|match|merge|national|nocheck|nonclustered|not|null|nullif|of|off|offsets|on|open|opendatasource|openquery|openrowset|openxml|option|or|order|outer|over|percent|plan|precision|primary|print|proc|procedure|public|raiserror|read|readtext|reconfigure|references|replication|restore|restrict|return|revoke|right|rollback|rowcount|rowguidcol|rule|save|schema|select|session_user|set|setuser|shutdown|some|statistics|system_user|table|textsize|then|to|top|tran|transaction|trigger|truncate|tsequal|union|unique|update|updatetext|use|user|using|values|varying|view|waitfor|when|where|while|with|writetext)(?=[^\w-]|$)/i,
null],["lit",/^[+-]?(?:0x[\da-f]+|(?:\.\d+|\d+(?:\.\d*)?)(?:e[+-]?\d+)?)/i],["pln",/^[_a-z][\w-]*/i],["pun",/^[^\w\t\n\r "'\xa0][^\w\t\n\r "'+\xa0-]*/]]),["sql"]);