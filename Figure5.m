
%x axis
x=1:16;
%locations of error bars
xx=x-0.2275;
data=[0.8708396 0.8778842 0.8892016 0.8672717 0.8763290 0.8392405 0.8849168 0.8851376 0.8905846 0.8668217 0.8819483 0.8926337 0.8866855 0.8835662 0.8878310 0.8738608];

data1=[0.00243038	0.010481013	11.51983122	0.00243038	11.44388186	3.178902954	3.178902954	0 3.178902954	0.00243038	11.51983122	11.51983122	10.31523207	10.31523207	10.31523207	10.31523207
];


cilower= [0.855325000578686 0.861278450823783 0.874835617914527 0.855698038382231 0.859164406956605 0.818229374645805 0.868401136979585   0.872540455281659       0.876217769371621       0.852151105449759       0.867246229278717       0.879210923784388       0.871284826671955       0.8683508752666       0.871648821063759       0.85581034832091]

ciupper=[0.888756478553516   0.896839040844578   0.906091530555427   0.87958790851789   0.896707694067949   0.860753530123801   0.902075776751317   0.8994658996   0.907442034253955   0.884262629208305   0.899542378796108   0.906984367137401   0.904850778666379   0.898909999620421   0.905847259033596   0.893899826945089]
data2=[2338.75  1537.38  2311.11  2404.48  2204.39    24.90 17260.28  0.36 11968.53  2034.85  2740.85  2007.60  2548.12  1535.27  3287.94  3804.10
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
xticklabels({"A" "B" "C_*" "D" "E" "F_*"  "G_*"  "H" "I" "J" "K_*" "L" "M" "N" "O" "P"});
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
      
     