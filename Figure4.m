x=1:10;
data=[0.030000 0.140000 0.0500000 121.6600000 4.7500000 22.1400000 44.0500000 69.0100000 206.6900000 26.5600000];
data=data/0.03; %normalise by fastest time
data1=[0.531626 0.711818 0.5188968   0.8187176  0.7124377  0.7991409  0.8222959  0.8252262   0.8352795 0.7333532];
yyaxis left;

left_color = [0 0 0];
right_color = [0 0 0];
set(0,'defaultAxesColorOrder',[left_color; right_color;left_color]);
% the trick to using different y axis scales on the same bar chart is the
% dummy data. Put the real data to the left for the left axis bar and to
% the right for the right axis bar.
dummydata=[0 0 0 0 0 0 0 0 0 0];
totaldata=[data1(:), dummydata(:)];
timedata=[dummydata(:),data(:)]


h1=bar(x,totaldata,'FaceColor',[0.6,0.6,0.6],'LineWidth',2);
ylabel('Accuracy R^{2}');
set(gca,"YLim",[0.4 0.9]);

set(gca,'LineWidth',3,'TickLength',[0.02,0.01],'TickDir','out');
set(gca,'FontSize',16);
box off;


xticklabels({"mean" "BC mean" "median" "RF" "PMM 1" "PMM 5"  "PMM 10"  "PMM 15" "PMM 50" "BPCA"});
xtickangle(45);
ycolor="black";


yyaxis right;
yticks('auto');
h2=bar(x,timedata,'FaceColor',[0.2,0.2,0.2],'LineWidth',2);

set(gca, 'YScale', 'log');
set(gca,'LineWidth',3,'TickLength',[0.00,0.00],'TickDir','out');
ycolor="black";
ylabel("Computation time, normalised");
h=legend([h1(1) h2(1)],{'Accuracy','Time (log)'});
