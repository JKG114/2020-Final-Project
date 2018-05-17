% C: an nx5 table of probabilities. Each row is five non-negative numbers
% that add up to 1, representing the proportions of the five ethnicities in
% one state. There are as 47rows(i.e., after removing unknown values and 
% rows with na values there were 47 states with recorded police killings.).
% Also note, that some state census tract rows had na values, and were
% disregarded

% E: same as C, except that the proportions come from the population of
% victims for the state. Rows in E correspond to the rows in C, i.e. they 
% come from the same state.

% Define a statistic 'S', which is the L1 distance between E and C

% Build a null distribution for S by creating "surrogate" versions of E.
% For example, let SE be an nx5 table with rows that correspond to the rows
% of C and E, except that the entries are random and come from random
% samples from the distributions represented in C. Each row of SE is
% determined from 'ESums' selections from the C distribution, where ESums
% is the number of victims recorded in that state.

% Get the data (Tract data and victim data)

C1=csvread('RacesOfStates.csv',1);
E1=csvread('RacesOfVictims_by_state.csv',1);
StateNames1 = table2array(readtable('StateNames.csv'));
ax1 = subplot(47,8,6);


for i = 1:length(C1)
    C = C1(i,:);
    E = E1(i,:);
    SNS = StateNames1(i);
    NumSamples=1000;
    [NumTracts,NTypes]=size(C);

    % Clean C, meaning force total in each row to be 1

    CSums=sum(C,2);
    for col=1:NTypes
        C(:,col)=C(:,col)./CSums;
    end

    % Get number of attacks in each row, and then normalize rows of E

    ESums=sum(E,2);
    for col=1:NTypes
        E(:,col)=E(:,col)./ESums;
    end

    S=sum(sum(abs(E-C))); % The observed value of the statistic

    % Make surrogate NumSamples surrogate E's and compute, for each, a
    % surrogate S ('SS')

    SS=zeros(NumSamples,1);

    for samp=1:NumSamples

        ES=zeros(NumTracts,NTypes);  % ES will hold the current surrogate E

        for row=1:NumTracts
            r = mnrnd(ESums(row),C(row,:)); % Multinomial selection of 
                        % number of victims, but from the distribution in C

            % load the victim numbers into the surrogate, ES
            for col=1:NTypes
                ES(row,col)=r(col); 
            end
        end

        % Normalize the rows of ES (make them probability distributions
        ESSums=sum(ES,2); 
        for col=1:NTypes
            ES(:,col)=ES(:,col)./ESSums;
        end

        % Compute the L1 distance between C and the surrogate ES
        SS(samp)=sum(sum(abs(ES-C)));

    end

    % Display Results
    hold on
    figure(i)
    close(i)
    figure(i)
    hist(SS);
    hold on
    scatter(S,0,100,'filled','r')
    hold off
    pvalue=(sum(SS>=S)+1)/(NumSamples+1);
    disp(['p-value: ',num2str(pvalue)]) 
    title(SNS)
end