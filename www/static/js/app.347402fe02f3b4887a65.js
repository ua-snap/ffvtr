webpackJsonp([1],[,,,,function(t,e,i){i(23);var o=i(1)(i(17),i(35),null,null);t.exports=o.exports},function(t,e,i){i(27);var o=i(1)(i(18),i(39),"data-v-8de79fee",null);t.exports=o.exports},,,function(t,e,i){"use strict";var o=i(3),a=i(41);o.a.use(a.a),e.a=new a.a({routes:[{path:"/:studentFtePercentChange/:tuitionFeesFTE2018/:tuitionFeesFTE2019/:tuitionFeesFTE2020/:tuitionFeesFTE2025/:totalStateAppropriation2018/:totalStateAppropriation2019/:totalStateAppropriation2020/:totalStateAppropriation2025",name:"edited"}]})},function(t,e,i){"use strict";function o(){return{studentFtePercentChange:2,tuitionFeesFTE2018:7e3,tuitionFeesFTE2019:7500,tuitionFeesFTE2020:8e3,tuitionFeesFTE2025:10089,totalStateAppropriation2018:340,totalStateAppropriation2019:330,totalStateAppropriation2020:333,totalStateAppropriation2025:312}}var a=i(3),n=i(60),r=i(2),s=i.n(r);a.a.use(n.a);e.a=new n.a.Store({strict:!1,state:o(),mutations:{update:function(t,e){t[e.field]=e.value},reset:function(t){s.a.extend(t,o())}}})},function(t,e,i){i(25);var o=i(1)(i(14),i(37),null,null);t.exports=o.exports},,,function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var o=i(3),a=i(11),n=i.n(a),r=i(10),s=i.n(r),p=i(12),u=(i.n(p),i(8)),l=i(9);o.a.use(n.a),o.a.config.productionTip=!1,i.i(p.sync)(l.a,u.a),new o.a({el:"#app",store:l.a,router:u.a,template:"<App/>",components:{App:s.a}})},function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var o=i(33),a=i.n(o),n=i(4),r=i.n(n),s=i(5),p=i.n(s),u=i(31),l=i.n(u),F=i(30),d=i.n(F),c=i(2),h=i.n(c);e.default={name:"app",components:{Spreadsheet:a.a,ResetButton:r.a,ShareButton:p.a,Graph:l.a,AppropriationsGraph:d.a},created:function(){this.restoreValuesFromUrl(["studentFtePercentChange","tuitionFeesFTE2018","tuitionFeesFTE2019","tuitionFeesFTE2020","tuitionFeesFTE2025","totalStateAppropriation2018","totalStateAppropriation2019","totalStateAppropriation2020","totalStateAppropriation2025"])},data:function(){return{studentFte2016:19229,studentFtePercentChangeStart:2,studentFtePercentChangeMin:-5,studentFtePercentChangeMax:5,tuitionFeesFTE2016:6806,tuitionFeesFTE2018start:7e3,tuitionFeesFTE2018Min:5e3,tuitionFeesFTE2018Max:15e3,tuitionFeesFTE2019start:7500,tuitionFeesFTE2019Min:5e3,tuitionFeesFTE2019Max:15e3,tuitionFeesFTE2020start:8e3,tuitionFeesFTE2020Min:5e3,tuitionFeesFTE2020Max:15e3,tuitionFeesFTE2025start:10089,tuitionFeesFTE2025Min:5e3,tuitionFeesFTE2025Max:15e3,totalStateAppropriation2016:350,totalStateAppropriation2018Min:200,totalStateAppropriation2018Max:500,totalStateAppropriation2018start:340,totalStateAppropriation2019Min:200,totalStateAppropriation2019Max:500,totalStateAppropriation2019start:330,totalStateAppropriation2020Min:200,totalStateAppropriation2020Max:500,totalStateAppropriation2020start:323,totalStateAppropriation2025Min:200,totalStateAppropriation2025Max:500,totalStateAppropriation2025start:318,totalTuitionFees2016:130.9,revenueEducationCost2016:480.9}},computed:{studentFte2018:function(){return this.interpolateStudentFte(2018)},studentFte2019:function(){return this.interpolateStudentFte(2019)},studentFte2020:function(){return this.interpolateStudentFte(2020)},studentFte2025:function(){return this.interpolateStudentFte(2025)},studentFtePercentChange:function(){return this.$store.state.studentFtePercentChange},tuitionFeesFTE2018:function(){return this.$store.state.tuitionFeesFTE2018},tuitionFeesFTE2019:function(){return this.$store.state.tuitionFeesFTE2019},tuitionFeesFTE2020:function(){return this.$store.state.tuitionFeesFTE2020},tuitionFeesFTE2025:function(){return this.$store.state.tuitionFeesFTE2025},totalStateAppropriation2018:function(){return this.$store.state.totalStateAppropriation2018},totalStateAppropriation2019:function(){return this.$store.state.totalStateAppropriation2019},totalStateAppropriation2020:function(){return this.$store.state.totalStateAppropriation2020},totalStateAppropriation2025:function(){return this.$store.state.totalStateAppropriation2025},stateAppropriationPerFTE2016:function(){return this.computeStateAppropriationPerFTE(this.studentFte2016,this.totalStateAppropriation2016)},stateAppropriationPerFTE2018:function(){return this.computeStateAppropriationPerFTE(this.studentFte2018,this.totalStateAppropriation2018)},stateAppropriationPerFTE2019:function(){return this.computeStateAppropriationPerFTE(this.studentFte2019,this.totalStateAppropriation2019)},stateAppropriationPerFTE2020:function(){return this.computeStateAppropriationPerFTE(this.studentFte2020,this.totalStateAppropriation2020)},stateAppropriationPerFTE2025:function(){return this.computeStateAppropriationPerFTE(this.studentFte2025,this.totalStateAppropriation2025)},totalTuitionFees2018:function(){return this.computeTotalTuitionFees(this.tuitionFeesFTE2018,this.studentFte2018)},totalTuitionFees2019:function(){return this.computeTotalTuitionFees(this.tuitionFeesFTE2019,this.studentFte2019)},totalTuitionFees2020:function(){return this.computeTotalTuitionFees(this.tuitionFeesFTE2020,this.studentFte2020)},totalTuitionFees2025:function(){return this.computeTotalTuitionFees(this.tuitionFeesFTE2025,this.studentFte2025)},revenueEducationCost2018:function(){return this.computeRevenueEducationCost(this.totalTuitionFees2018,this.totalStateAppropriation2018)},revenueEducationCost2019:function(){return this.computeRevenueEducationCost(this.totalTuitionFees2019,this.totalStateAppropriation2019)},revenueEducationCost2020:function(){return this.computeRevenueEducationCost(this.totalTuitionFees2020,this.totalStateAppropriation2020)},revenueEducationCost2025:function(){return this.computeRevenueEducationCost(this.totalTuitionFees2025,this.totalStateAppropriation2025)}},methods:{computeStateAppropriationPerFTE:function(t,e){return parseFloat((1e6*parseFloat(e)/parseFloat(t)).toFixed(2))},computeRevenueEducationCost:function(t,e){return parseFloat((parseFloat(t)+parseFloat(e)).toFixed(2))},computeTotalTuitionFees:function(t,e){return parseFloat((t*e/1e6).toFixed(2))},restoreValuesFromUrl:function(t){var e=this;h.a.each(t,function(t){e.$route.params[t]&&e.setStoreValue(t,e.validate(t,e.$route.params[t]))})},setStoreValue:function(t,e){this.$store.commit("update",{field:t,value:e})},interpolateStudentFte:function(t){var e=parseFloat(this.studentFtePercentChange),i=1+.01*e,o=t-2016;return Math.floor(this.studentFte2016*Math.pow(i,o))},validate:function(t,e){var i=parseInt(e);return!0===h.a.isNaN(i)&&(i=0),i>=this[t+"Min"]&&i<=this[t+"Max"]?i:i<this[t+"Min"]?this[t+"Min"]:this[t+"Max"]},updated:function(t,e){this.setStoreValue(t,e),this.$router.push({name:"edited",params:{studentFtePercentChange:this.studentFtePercentChange,tuitionFeesFTE2018:this.tuitionFeesFTE2018,tuitionFeesFTE2019:this.tuitionFeesFTE2019,tuitionFeesFTE2020:this.tuitionFeesFTE2020,tuitionFeesFTE2025:this.tuitionFeesFTE2025,totalStateAppropriation2018:this.totalStateAppropriation2018,totalStateAppropriation2019:this.totalStateAppropriation2019,totalStateAppropriation2020:this.totalStateAppropriation2020,totalStateAppropriation2025:this.totalStateAppropriation2025}})},resetDefaults:function(t){this.$store.commit("reset")}}}},function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var o=i(2),a=i.n(o),n=function(t,e,i,o){return t=parseFloat(t,2),((e=parseFloat(e,2))-t)/i*(parseInt(this)-o)+t},r=[2016,2018,2019,2020,2021,2022,2023,2024,2025];e.default={name:"appropriations-graph",props:["stateAppropriationPerFTE2016","stateAppropriationPerFTE2018","stateAppropriationPerFTE2019","stateAppropriationPerFTE2020","stateAppropriationPerFTE2025"],watch:{stateAppropriationPerFTE2016:function(){this.refreshPlot()},stateAppropriationPerFTE2018:function(){this.refreshPlot()},stateAppropriationPerFTE2019:function(){this.refreshPlot()},stateAppropriationPerFTE2020:function(){this.refreshPlot()},stateAppropriationPerFTE2025:function(){this.refreshPlot()}},mounted:function(){this.graph=document.getElementById("graph2"),this.drawPlot()},methods:{calcInterp:function(t,e){return(t+e)/2},drawPlot:function(){window.Plotly.plot(this.graph,this.getGraphData(),this.getGraphLayout())},getGraphLayout:function(){return{title:"State Appropriations per FTE",margin:{l:60,r:60,b:60,t:60},barmode:"stack",xaxis:{type:"category"},yaxis:{title:"Thousand $",hoverformat:",f6",range:[1e4,2e4]},legend:{orientation:"h",font:{family:"Lato"}}}},processData:function(t,e,i){return a.a.concat(this[t+"2016"],this[t+"2018"],this[t+"2019"],this[t+"2020"],a.a.invokeMap([2021,2022,2023,2024],n,this[t+"2020"],this[t+"2025"],5,2020),this[t+"2025"])},getGraphData:function(){return[{x:r,y:this.processData("stateAppropriationPerFTE",this.stateAppropriationPerFTE2020,this.stateAppropriationPerFTE2025),name:"State Appropriations per FTE",type:"bar",marker:{color:"#13AD1B"}}]},refreshPlot:function(){this.graph.data=this.getGraphData(),window.Plotly.redraw(this.graph)}}}},function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var o=i(2),a=i.n(o),n=function(t,e,i,o){t=parseFloat(t,2),e=parseFloat(e,2);var a=parseInt(this);return Math.round((e-t)/i*(a-o)+t)},r=[2016,2018,2019,2020,2021,2022,2023,2024,2025];e.default={name:"graph",props:["totalTuitionFees2016","totalTuitionFees2018","totalTuitionFees2019","totalTuitionFees2020","totalTuitionFees2025","totalStateAppropriation2016","totalStateAppropriation2018","totalStateAppropriation2019","totalStateAppropriation2020","totalStateAppropriation2025","studentFte2016","studentFte2018","studentFte2019","studentFte2020","studentFte2025"],watch:{totalTuitionFees2016:function(){this.refreshPlot()},totalTuitionFees2018:function(){this.refreshPlot()},totalTuitionFees2019:function(){this.refreshPlot()},totalTuitionFees2020:function(){this.refreshPlot()},totalTuitionFees2025:function(){this.refreshPlot()},totalStateAppropriation2016:function(){this.refreshPlot()},totalStateAppropriation2018:function(){this.refreshPlot()},totalStateAppropriation2019:function(){this.refreshPlot()},totalStateAppropriation2020:function(){this.refreshPlot()},totalStateAppropriation2025:function(){this.refreshPlot()},studentFte2025:function(){this.refreshPlot()}},mounted:function(){this.graph=document.getElementById("graph1"),this.drawPlot()},methods:{calcInterp:function(t,e){return(t+e)/2},drawPlot:function(){window.Plotly.plot(this.graph,this.getGraphData(),this.getGraphLayout())},getGraphLayout:function(){return{title:"Enrollment, Tuition & Fees, State Appropriations",barmode:"stack",margin:{l:60,r:60,b:60,t:60},xaxis:{type:"category"},yaxis:{title:"Million $",hoverformat:"$,f6",range:[0,700]},yaxis2:{title:"Enrollment (FTE)",side:"right",overlaying:"y",range:[1e4,3e4],hoverformat:",."},legend:{orientation:"h",font:{family:"Lato"}}}},processData:function(t,e,i){return a.a.concat(this[t+"2016"],this[t+"2018"],this[t+"2019"],this[t+"2020"],a.a.invokeMap([2021,2022,2023,2024],n,this[t+"2020"],this[t+"2025"],5,2020),this[t+"2025"])},getGraphData:function(){return[{x:r,y:this.processData("totalStateAppropriation",this.totalStateAppropriation2020,this.totalStateAppropriation2025),name:"Appropriations",type:"bar",marker:{color:"#4575B5"}},{x:r,y:this.processData("totalTuitionFees",this.totalTuitionFees2020,this.totalTuitionFees2025),name:"Tuition & Fees",type:"bar",marker:{color:"#E3593D"}},{x:r,y:this.processData("studentFte",this.studentFte2020,this.studentFte2025),name:"Enrollment",yaxis:"y2",type:"scatter",line:{color:"#333",width:3},marker:{color:"#000",size:7}}]},refreshPlot:function(){this.graph.data=this.getGraphData(),window.Plotly.redraw(this.graph)}}}},function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0}),e.default={name:"reset-button",methods:{resetDefaults:function(){this.$emit("resetdefaults"),this.$router.replace("/")}}}},function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0}),e.default={name:"share-button",data:function(){return{showUrl:!1,shareText:"Share URL"}},computed:{currentUrl:function(){return window.location.origin+window.location.pathname+"#"+this.$route.fullPath}},methods:{toggleShare:function(){this.showUrl=!this.showUrl,this.shareText=this.showUrl?"Hide URL":"Share URL"}}}},function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var o=i(21),a=i.n(o),n=i(2),r=i.n(n),s=i(48);i.n(s);e.default={name:"slider-input",props:{id:String,min:Number,max:Number,start:Number,currvalue:[Number,String],step:{type:Number,default:1}},data:function(){return{value:void 0}},created:function(){this.value=this.currvalue},mounted:function(){var t=this;t.slider=new a.a("#"+t.id,{step:this.step,ticks:[this.min,this.start,this.max],tooltip_position:"bottom"}),t.slider.handle1.removeAttribute("tabindex"),t.slider.handle2.removeAttribute("tabindex"),t.slider.on("change",function(e){t.value=e.newValue,t.$emit("updated",t.id,t.value)}),t.slider.setValue(this.value)},validations:{value:{between:function(t){return!r.a.isNaN(parseFloat(t))&&i.i(s.between)(this.min,this.max)(t)}}},methods:{updateValue:function(t,e){var i=parseFloat(t);e&&(i>this.max?t=this.max:i<this.min&&(t=this.min),this.value=t,this.slider.setValue(t),this.$emit("updated",this.id,t))}},watch:{currvalue:function(t){this.value=t,this.slider.setValue(t)}}}},function(t,e,i){"use strict";Object.defineProperty(e,"__esModule",{value:!0});var o=i(32),a=i.n(o),n=i(4),r=i.n(n),s=i(5),p=i.n(s);e.default={name:"spreadsheet",components:{SliderInput:a.a,ShareButton:p.a,ResetButton:r.a},filters:{money:function(t){return Math.round(t).toLocaleString()}},props:["studentFte2016","studentFte2018","studentFte2019","studentFte2020","studentFte2025","studentFtePercentChange","studentFtePercentChangeStart","studentFtePercentChangeMin","studentFtePercentChangeMax","tuitionFeesFTE2016","tuitionFeesFTE2018","tuitionFeesFTE2018start","tuitionFeesFTE2018Min","tuitionFeesFTE2018Max","tuitionFeesFTE2019","tuitionFeesFTE2019start","tuitionFeesFTE2019Min","tuitionFeesFTE2019Max","tuitionFeesFTE2020","tuitionFeesFTE2020start","tuitionFeesFTE2020Min","tuitionFeesFTE2020Max","tuitionFeesFTE2025","tuitionFeesFTE2025start","tuitionFeesFTE2025Min","tuitionFeesFTE2025Max","totalStateAppropriation2016","totalStateAppropriation2018","totalStateAppropriation2018start","totalStateAppropriation2018Min","totalStateAppropriation2018Max","totalStateAppropriation2019","totalStateAppropriation2019start","totalStateAppropriation2019Min","totalStateAppropriation2019Max","totalStateAppropriation2020","totalStateAppropriation2020start","totalStateAppropriation2020Min","totalStateAppropriation2020Max","totalStateAppropriation2025","totalStateAppropriation2025start","totalStateAppropriation2025Min","totalStateAppropriation2025Max","stateAppropriationPerFTE2016","stateAppropriationPerFTE2018","stateAppropriationPerFTE2019","stateAppropriationPerFTE2020","stateAppropriationPerFTE2025","totalTuitionFees2016","totalTuitionFees2018","totalTuitionFees2019","totalTuitionFees2020","totalTuitionFees2025","revenueEducationCost2016","revenueEducationCost2018","revenueEducationCost2019","revenueEducationCost2020","revenueEducationCost2025"],methods:{updated:function(t,e){this.$emit("updated",t,e)}}}},,function(t,e){},function(t,e){},function(t,e){},function(t,e){},function(t,e){},function(t,e){},function(t,e){},,function(t,e,i){i(28);var o=i(1)(i(15),i(40),"data-v-d4588902",null);t.exports=o.exports},function(t,e,i){i(26);var o=i(1)(i(16),i(38),"data-v-7aaebae6",null);t.exports=o.exports},function(t,e,i){i(24);var o=i(1)(i(19),i(36),null,null);t.exports=o.exports},function(t,e,i){i(22);var o=i(1)(i(20),i(34),"data-v-27866664",null);t.exports=o.exports},function(t,e){t.exports={render:function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("div",{staticClass:"spreadsheet"},[i("table",{staticClass:"table"},[t._m(0),t._v(" "),i("tbody",[i("tr",[i("th",{attrs:{scope:"row"}},[t._v("Student FTE")]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.studentFte2016)))]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.studentFte2018)))]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.studentFte2019)))]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.studentFte2020)))]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.studentFte2025)))])]),t._v(" "),i("tr",[i("th",{attrs:{scope:"row"}},[t._v("% change FTE per year")]),t._v(" "),i("td",{attrs:{colspan:"5"}},[i("slider-input",{staticClass:"fte-percent-change",attrs:{id:"studentFtePercentChange",min:t.studentFtePercentChangeMin,max:t.studentFtePercentChangeMax,start:t.studentFtePercentChangeStart,currvalue:t.studentFtePercentChange,step:.1},on:{updated:t.updated}})],1)]),i("tr",[i("th",{attrs:{scope:"row"}},[t._v("Tuition and Fees per Student FTE ($)")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.tuitionFeesFTE2016))+"\n        ")]),t._v(" "),i("td",[i("slider-input",{attrs:{id:"tuitionFeesFTE2018",min:t.tuitionFeesFTE2018Min,max:t.tuitionFeesFTE2018Max,start:t.tuitionFeesFTE2018start,currvalue:t.tuitionFeesFTE2018,step:1},on:{updated:t.updated}})],1),t._v(" "),i("td",[i("slider-input",{attrs:{id:"tuitionFeesFTE2019",min:t.tuitionFeesFTE2019Min,max:t.tuitionFeesFTE2019Max,start:t.tuitionFeesFTE2019start,currvalue:t.tuitionFeesFTE2019},on:{updated:t.updated}})],1),t._v(" "),i("td",[i("slider-input",{attrs:{id:"tuitionFeesFTE2020",min:t.tuitionFeesFTE2020Min,max:t.tuitionFeesFTE2020Max,start:t.tuitionFeesFTE2020start,currvalue:t.tuitionFeesFTE2020},on:{updated:t.updated}})],1),t._v(" "),i("td",[i("slider-input",{attrs:{id:"tuitionFeesFTE2025",min:t.tuitionFeesFTE2025Min,max:t.tuitionFeesFTE2025Max,start:t.tuitionFeesFTE2025start,currvalue:t.tuitionFeesFTE2025},on:{updated:t.updated}})],1)]),t._v(" "),i("tr",[i("th",{attrs:{scope:"row"}},[t._v("Total State Appropriation (Million $)")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.totalStateAppropriation2016))+"\n        ")]),t._v(" "),i("td",[i("slider-input",{attrs:{id:"totalStateAppropriation2018",min:t.totalStateAppropriation2018Min,max:t.totalStateAppropriation2018Max,start:t.totalStateAppropriation2018start,currvalue:t.totalStateAppropriation2018},on:{updated:t.updated}})],1),t._v(" "),i("td",[i("slider-input",{attrs:{id:"totalStateAppropriation2019",min:t.totalStateAppropriation2019Min,max:t.totalStateAppropriation2019Max,start:t.totalStateAppropriation2019start,currvalue:t.totalStateAppropriation2019},on:{updated:t.updated}})],1),t._v(" "),i("td",[i("slider-input",{attrs:{id:"totalStateAppropriation2020",min:t.totalStateAppropriation2020Min,max:t.totalStateAppropriation2020Max,start:t.totalStateAppropriation2020start,currvalue:t.totalStateAppropriation2020},on:{updated:t.updated}})],1),t._v(" "),i("td",[i("slider-input",{attrs:{id:"totalStateAppropriation2025",min:t.totalStateAppropriation2025Min,max:t.totalStateAppropriation2025Max,start:t.totalStateAppropriation2025start,currvalue:t.totalStateAppropriation2025},on:{updated:t.updated}})],1)]),t._v(" "),i("tr",[i("th",{attrs:{scope:"row"}},[t._v("State Appropriation per FTE ($)")]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.stateAppropriationPerFTE2016)))]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.stateAppropriationPerFTE2018))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.stateAppropriationPerFTE2019))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.stateAppropriationPerFTE2020))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.stateAppropriationPerFTE2025))+"\n        ")])]),t._v(" "),i("tr",[i("th",{attrs:{scope:"row"}},[t._v("Total Tuition & Fees (Million $)")]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.totalTuitionFees2016)))]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.totalTuitionFees2018))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.totalTuitionFees2019))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.totalTuitionFees2020))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.totalTuitionFees2025))+"\n        ")])]),t._v(" "),i("tr",[i("th",{attrs:{scope:"row"}},[t._v("Revenue, Educational Cost (Million $)")]),t._v(" "),i("td",[t._v(t._s(t._f("money")(t.revenueEducationCost2016)))]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.revenueEducationCost2018))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.revenueEducationCost2019))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.revenueEducationCost2020))+"\n        ")]),t._v(" "),i("td",[t._v("\n          "+t._s(t._f("money")(t.revenueEducationCost2025))+"\n        ")])])])])])},staticRenderFns:[function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("thead",[i("tr",[i("th",{attrs:{scope:"col"}}),t._v(" "),i("th",{attrs:{scope:"col"}},[t._v("2016")]),t._v(" "),i("th",{attrs:{scope:"col"}},[t._v("FY2018")]),t._v(" "),i("th",{attrs:{scope:"col"}},[t._v("FY2019")]),t._v(" "),i("th",{attrs:{scope:"col"}},[t._v("FY2020")]),t._v(" "),i("th",{attrs:{scope:"col"}},[t._v("FY2025")])])])}]}},function(t,e){t.exports={render:function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("div",[i("button",{staticClass:"btn",attrs:{type:"submit"},on:{click:t.resetDefaults}},[t._v("Reset")])])},staticRenderFns:[]}},function(t,e){t.exports={render:function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("div",[i("div",{staticClass:"form-group",class:{"form-group--error":t.$v.value.$error}},[i("input",{directives:[{name:"model",rawName:"v-model:trim",value:t.value,expression:"value",arg:"trim"},{name:"model",rawName:"v-model:value",value:t.value,expression:"value",arg:"value"}],ref:"input",staticClass:"form__input input",domProps:{value:t.value,value:t.value},on:{input:[function(e){e.target.composing||(t.value=e.target.value)},function(e){e.target.composing||(t.value=e.target.value)},function(e){t.updateValue(e.target.value,t.$v.value.between)}]}}),t._v(" "),i("input",{staticClass:"slider",attrs:{id:t.id,type:"text","data-slider-min":t.min,"data-slider-max":t.max,"data-slider-value":t.start}})]),t._v(" "),t.$v.value.between?t._e():i("span",{staticClass:"form-group__message"},[t._v("Must be between "+t._s(t.$v.value.$params.between.min)+" and "+t._s(t.$v.value.$params.between.max))])])},staticRenderFns:[]}},function(t,e){t.exports={render:function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("div",{staticClass:"container-fluid"},[i("h1",[t._v("UA Financial Framework Visualization Tool")]),t._v(" "),i("div",{staticClass:"row"},[i("div",{staticClass:"col-md-6"},[i("spreadsheet",{attrs:{studentFte2016:t.studentFte2016,studentFte2018:t.studentFte2018,studentFte2019:t.studentFte2019,studentFte2020:t.studentFte2020,studentFte2025:t.studentFte2025,studentFtePercentChange:t.studentFtePercentChange,studentFtePercentChangeStart:t.studentFtePercentChangeStart,studentFtePercentChangeMin:t.studentFtePercentChangeMin,studentFtePercentChangeMax:t.studentFtePercentChangeMax,tuitionFeesFTE2016:t.tuitionFeesFTE2016,tuitionFeesFTE2018:t.tuitionFeesFTE2018,tuitionFeesFTE2018start:t.tuitionFeesFTE2018start,tuitionFeesFTE2018Min:t.tuitionFeesFTE2018Min,tuitionFeesFTE2018Max:t.tuitionFeesFTE2018Max,tuitionFeesFTE2019:t.tuitionFeesFTE2019,tuitionFeesFTE2019start:t.tuitionFeesFTE2019start,tuitionFeesFTE2019Min:t.tuitionFeesFTE2019Min,tuitionFeesFTE2019Max:t.tuitionFeesFTE2019Max,tuitionFeesFTE2020:t.tuitionFeesFTE2020,tuitionFeesFTE2020start:t.tuitionFeesFTE2020start,tuitionFeesFTE2020Min:t.tuitionFeesFTE2020Min,tuitionFeesFTE2020Max:t.tuitionFeesFTE2020Max,tuitionFeesFTE2025:t.tuitionFeesFTE2025,tuitionFeesFTE2025start:t.tuitionFeesFTE2025start,tuitionFeesFTE2025Min:t.tuitionFeesFTE2025Min,tuitionFeesFTE2025Max:t.tuitionFeesFTE2025Max,totalStateAppropriation2016:t.totalStateAppropriation2016,totalStateAppropriation2018:t.totalStateAppropriation2018,totalStateAppropriation2018start:t.totalStateAppropriation2018start,totalStateAppropriation2018Min:t.totalStateAppropriation2018Min,totalStateAppropriation2018Max:t.totalStateAppropriation2018Max,totalStateAppropriation2019:t.totalStateAppropriation2019,totalStateAppropriation2019start:t.totalStateAppropriation2019start,totalStateAppropriation2019Min:t.totalStateAppropriation2019Min,totalStateAppropriation2019Max:t.totalStateAppropriation2019Max,totalStateAppropriation2020:t.totalStateAppropriation2020,totalStateAppropriation2020start:t.totalStateAppropriation2020start,totalStateAppropriation2020Min:t.totalStateAppropriation2020Min,totalStateAppropriation2020Max:t.totalStateAppropriation2020Max,totalStateAppropriation2025:t.totalStateAppropriation2025,totalStateAppropriation2025start:t.totalStateAppropriation2025start,totalStateAppropriation2025Min:t.totalStateAppropriation2025Min,totalStateAppropriation2025Max:t.totalStateAppropriation2025Max,stateAppropriationPerFTE2016:t.stateAppropriationPerFTE2016,stateAppropriationPerFTE2018:t.stateAppropriationPerFTE2018,stateAppropriationPerFTE2019:t.stateAppropriationPerFTE2019,stateAppropriationPerFTE2020:t.stateAppropriationPerFTE2020,stateAppropriationPerFTE2025:t.stateAppropriationPerFTE2025,totalTuitionFees2016:t.totalTuitionFees2016,totalTuitionFees2018:t.totalTuitionFees2018,totalTuitionFees2019:t.totalTuitionFees2019,totalTuitionFees2020:t.totalTuitionFees2020,totalTuitionFees2025:t.totalTuitionFees2025,revenueEducationCost2016:t.revenueEducationCost2016,revenueEducationCost2018:t.revenueEducationCost2018,revenueEducationCost2019:t.revenueEducationCost2019,revenueEducationCost2020:t.revenueEducationCost2020,revenueEducationCost2025:t.revenueEducationCost2025},on:{updated:t.updated}}),t._v(" "),i("reset-button",{attrs:{id:"reset"},on:{resetdefaults:t.resetDefaults}}),t._v(" "),i("share-button",{attrs:{id:"share"}})],1),t._v(" "),i("div",{staticClass:"col-md-6"},[i("graph",{attrs:{studentFte2016:t.studentFte2016,studentFte2018:t.studentFte2018,studentFte2019:t.studentFte2019,studentFte2020:t.studentFte2020,studentFte2025:t.studentFte2025,totalStateAppropriation2016:t.totalStateAppropriation2016,totalStateAppropriation2018:t.totalStateAppropriation2018,totalStateAppropriation2019:t.totalStateAppropriation2019,totalStateAppropriation2020:t.totalStateAppropriation2020,totalStateAppropriation2025:t.totalStateAppropriation2025,totalTuitionFees2016:t.totalTuitionFees2016,totalTuitionFees2018:t.totalTuitionFees2018,totalTuitionFees2019:t.totalTuitionFees2019,totalTuitionFees2020:t.totalTuitionFees2020,totalTuitionFees2025:t.totalTuitionFees2025}}),t._v(" "),i("appropriations-graph",{attrs:{stateAppropriationPerFTE2016:t.stateAppropriationPerFTE2016,stateAppropriationPerFTE2018:t.stateAppropriationPerFTE2018,stateAppropriationPerFTE2019:t.stateAppropriationPerFTE2019,stateAppropriationPerFTE2020:t.stateAppropriationPerFTE2020,stateAppropriationPerFTE2025:t.stateAppropriationPerFTE2025}})],1)])])},staticRenderFns:[]}},function(t,e){t.exports={render:function(){var t=this,e=t.$createElement;t._self._c;return t._m(0)},staticRenderFns:[function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("div",{staticClass:"graph"},[i("div",{attrs:{id:"graph1"}})])}]}},function(t,e){t.exports={render:function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("div",[i("button",{staticClass:"btn",attrs:{type:"submit"},on:{click:function(e){t.toggleShare()}}},[t._v(t._s(t.shareText))]),t._v(" "),t.showUrl?i("input",{attrs:{onclick:"this.select()"},domProps:{value:t.currentUrl}}):t._e()])},staticRenderFns:[]}},function(t,e){t.exports={render:function(){var t=this,e=t.$createElement;t._self._c;return t._m(0)},staticRenderFns:[function(){var t=this,e=t.$createElement,i=t._self._c||e;return i("div",{staticClass:"graph"},[i("div",{attrs:{id:"graph2"}})])}]}}],[13]);
//# sourceMappingURL=app.347402fe02f3b4887a65.js.map