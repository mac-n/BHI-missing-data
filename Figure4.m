
%x axis
x=1:16;
%locations of error bars
xx=x-0.2275;
data=[0.8708396 0.8778842 0.888 0.8672717 0.889 0.8392405 0.8849168 0.8851376 0.8905846 0.8668217 0.89 0.896 0.8866855 0.8835662 0.8878310 0.8738608];

data1=[0.00243038	0.010481013	11.883	0.00243038	11.883	3.178902954	3.178902954	0 3.178902954	0.00243038	11.883	11.8	10.31523207	10.31523207	10.31523207	10.31523207
];


cilower= [0.855325000578686 0.861278450823783 0.8733 0.855698038382231 0.8755 0.818229374645805 0.868401136979585   0.872540455281659       0.876217769371621       0.852151105449759       0.8739       0.8819       0.871284826671955       0.8683508752666       0.871648821063759       0.85581034832091]

ciupper=[0.888756478553516   0.896839040844578   0.9044   0.87958790851789   0.9033   0.860753530123801   0.902075776751317   0.8994658996   0.907442034253955   0.884262629208305   0.9086   0.9092   0.904850778666379   0.898909999620421   0.905847259033596   0.893899826945089]
data2=[2338.75  1537.38  2553.91   2404.48  2239.91    24.90 17260.28  0.36 11968.53  2034.85  1490.28   1668.55    2548.12  1422.27  1448.94  1685.1
 ];

data2=data2/1185;
yyaxis left;


 left_color = [0 0 0];
right_color = [0 0 0];

set(0,'defaultAxesColorOrder',[left_color; right_color;left_color]);

%to have two y axes with different scales you need to use dummy
%columns which space the bars out. 
dummydata=[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ];
totaldata=[data(:), dummydata(:), dummydata(:)];


timedata=[dummydata(:),data1(:),data2(:)];

h=bar(x,totaldata,'FaceColor',[0.9,0.9,0.9],'LineWidth',0.5);

hold on;
 er=errorbar(xx,data,data-cilower,data-ciupper,'LineWidth',1);
er.Color = [0 0 0];                            
er.LineStyle = 'none';
er.Bar.LineStyle = 'dotted';
%er.CapSize = 5;
hold off;
ylabel('Multiclass AUC');
set(gca,"YLim",[0.82 0.92]);
set(gca,"XLim",[0 17]);
yl=yline(0.91,'--','Complete Data AUC RF/SVM');
yl.LabelHorizontalAlignment='left';
 yl.FontSize=14
 yl2=yline(0.89,'--','NB');
yl2.LabelHorizontalAlignment='left';
 yl2.FontSize=14
set(gca,'LineWidth',2,'TickLength',[0.02,0.01],'TickDir','out');
set(gca,'FontSize',16)
%set(gca,"ylabel","Accuracy R^{2}");
%colororder({'k','k','k'});
%set(fig,'defaultAxesColorOrder',[0 0 0; 0 0 0]);


xtickangle(0);
ycolor="black";

%bar(totaldata);
%bar(alldata);
yyaxis right;

hb=bar(x,timedata,'LineWidth',2);
xticks(x);
xticklabels({"A" "B" "C" "D" "E" "F*"  "G*"  "H" "I" "J" "K" "L" "M*" "N" "O*" "P"});
box off;

hb(2).FaceColor = [0.5 0.5 0.5];


hb(3).FaceColor = [0.1 0.1 0.1];
hold off
set(gca, 'YScale', 'log');
%set(gca,"YLim",[0 3]);
set(gca,'LineWidth',3,'TickLength',[0.00,0.00],'TickDir','out');
ycolor="black";
set(gca,'FontSize',16);
ylabel("Computation time, log");


legend([h(1),hb(2),hb(3)],{'Multiclass AUC','Imputation Time','Classification Time'},'NumColumns', 3);
      
     
