%Load all variables

clear variables;
start = tic;

fid = fopen('Input.txt');


fileID = fopen('output.txt','w');



tline = fgetl(fid); %Skip type

tline = fgetl(fid);
type = str2num(tline);

tline = fgetl(fid); %Skip m and n



tline = fgetl(fid);
%Extract the m, n from TopLine
names = split(tline);
dim = cellfun(@str2num,names);
om = dim(1);  %originals
on = dim(2);


tline = fgetl(fid); %Skip A

oA = zeros(om, on);

%Populate A
for i=1:om
   tline = fgetl(fid);
   namei = split(tline);
   constrainti = cellfun(@str2double,namei);
   for j=1:on
       oA(i, j) = constrainti(j);
   end
end
A = oA;

tline = fgetl(fid); %Skip b

ob = zeros(om, 1);

%Populate b
for i=1:om
   tline = fgetl(fid);
   namei = split(tline);
   constraintb = cellfun(@str2double,namei);
   ob(i, 1) = constraintb(1);
end
b = ob;

tline = fgetl(fid); %min or max

maxS = 0;
if(tline == "max")
    maxS = 1;
end

oC = zeros(on, 1);


%Added
for i=1:on
   tline = fgetl(fid);
   namei = split(tline);
   constraintb = cellfun(@str2double,namei);
   oC(i, 1) = constraintb(1);
end


%Get Constant
Constant = 0;
if ~feof(fid)   %Checks if constant present
    tline = fgetl(fid);
    namei = split(tline);
    constraintb = cellfun(@str2double,namei);
    Constant = constraintb(1);
end



oC = transpose(oC);


%Added

%{
%Populate C
tline = fgetl(fid);
namei = split(tline);
constraintc = cellfun(@str2double,namei);
for i = 1:on
    oC(1, i) = constraintc(i);
end
%}


if maxS == 1
    oC = -oC;
end

fclose(fid);

r = oC;

%Remove Unwanted Variables
clear i j namei constraintb constraintC constrainti dim names fid tline constraintc; 

C = r;
m = om;
n = on;

if type == 2  %Add m slack
    disp('Adding Slack variables, since type 2');
    fprintf(fileID, 'Adding Slack variables, since type 2\n\n');
    I = eye(om);
    A = [A I];
    r = [r zeros(1, om)];
    n = on + om;
    m = om;
    C = r;
elseif type == 3  %Add m surplus
    disp('Adding Surplus variables, since type 3');
    fprintf(fileID, 'Adding Surplus variables, since type 3\n\n');
    I = eye(om);
    I = -I;
    A = [A I];
    r = [r zeros(1, om)];
    n = on + om;
    m = om;
    C = r;
end

%Make sure b's are >= 0
for i=1:m
    if b(i) < 0   %Multiply row by -1, to make positive
        b(i) = -b(i);
        A(i,:) = -A(i,:);
    end
end



%Display all stored variables
disp("The standard form is:")
disp("A = ");
disp(A);
disp("b = ");
disp(b);
disp("C = ");
disp(C.');
%return




fprintf(fileID,'The standard form is:\n');
fprintf(fileID, "A = \n\t");
for ii = 1:size(A,1)
    fprintf(fileID,'%g\t',A(ii,:));
    fprintf(fileID,'\n\t');
end
fprintf(fileID, "\nb = \n\t");
for ii = 1:size(b,1)
    fprintf(fileID,'%g\t',b(ii,:));
    fprintf(fileID,'\n\t');
end
transposeC = C.';
fprintf(fileID, "\nC = \n\t");
for ii = 1:size(transposeC,1)
    fprintf(fileID,'%g\t',transposeC(ii,:));
    fprintf(fileID,'\n\t');
end
clear transposeC;


%Check which are the basic variables
basic = zeros(m, 1);
I = eye(m);
for i=1:n
    column = A(:, i);
    for j = 1:m
        if isequal(column, I(:, j))
            basic(j) = i;
            break
        end
    end
end

numA = 0;
Phase1 = 0;

for i=1:m        %Check if not enough basic variables
    if basic(i) == 0 
        numA = numA + 1; 
    end
end


if m>n
    fprintf('\nThe original problem is infeasible. (m > n)\n');
    fprintf(fileID, "\nThe original problem is infeasible. (m > n)\n");
    Elapsed_time = toc(start);
    fprintf(fileID, "\nTime Taken: %f seconds\n", Elapsed_time);
    fclose(fileID);
    clear variables;
    return
end




if numA > 0
    disp("Phase I required");
    fprintf(fileID, "\nPhase I required.\n");
    Phase1 = 1;
end

if Phase1 == 0
    disp("Phase I not required");
    fprintf(fileID, "\nPhase I not required.\n");
end

if Phase1
  
    %Add numA artificial variables, to use in Phase I
    n1 = n + numA;
    m1 = m;
    
    
    
    I = eye(m1);
    
    %Create Phase 1 A
    for i = 1:m1
        if basic(i) == 0
            A = [A I(:, i)];
        end
    end
    A1 = A;
    
    %Create Phase 1 Basis
    basic1 = zeros(m1, 1);
    for i=1:n1
        column = A1(:, i);
        for j = 1:m1
            if isequal(column, I(:, j))
                basic1(j) = i;
                break
            end
        end
    end
    %Set the Identity to the new artificial variables
    
    %Use A1 for Phase I

    %Keep b the same for Phase I
    b1 = b;

    %New Optimization function
    
    r1 = [zeros(1, n) ones(1, numA)];       %Add correctly
    
    %Start by converting the 1's to 0's in r1
    for i=1:m1
        r1 = r1 - A1(i, :);
    end
    
    
    
 
  %Delete This  
    disp("The initial Phase I tableau is:")
    disp("A = ");
    disp(A1);
    disp("b = ");
    disp(b1);
    disp("r = ");
    disp(r1);
    fprintf("The basis is initially:\n");
    for i = 1:m1
        fprintf("x%d ", basic1(i));
    end
    fprintf("\n");
   %Delete this 
   
    iteration = 0;
    
    while 1  %Each step of Phase 1 corresponds to 1 iteration
        
        [minimum, j] = min(r1);   %Pivot column

        if minimum >= 0  %No negative r, go to Phase II
            X = zeros(n1, 1);
            for index = 1:m1
                X(basic1(index)) = b1(index);
            end 
            
            Cost = [zeros(1, n)  ones(1, numA)];
            
            z = Cost * X;
            if  z > 0    %Print Infeasible
                fprintf('\nThe original problem is infeasible.\n');
                fprintf(fileID, "\nThe original problem is infeasible.\n");
                Elapsed_time = toc(start);
                fprintf(fileID, "\nTime Taken: %f seconds\n", Elapsed_time);
                fclose(fileID);
                clear variables;
                return
            end
            
            fprintf("\n\nPhase I Complete\n");
            fprintf(fileID, "\n\nPhase I Complete.\n");
            break
        else   %Find Pivot row and check column j for it
            iteration = iteration +1 ;
            fprintf("\n\nPhase I Iteration %d\n\n", iteration);
            fprintf(fileID, "\n\nPhase I Iteration %d\n\n", iteration);
            
            ratio_column = zeros(m1, 1);
            
            for i=1:m1
                ratio_column(i) = b1(i) / A1(i,j);
                if (A1(i,j) <= 0) || (ratio_column(i) <= 0)
                    ratio_column(i) = 1/0;   %If negative value of b or ratio then set to infinity
                end
            end

            [minimum, i] = min(ratio_column);
            if minimum <= 0      %No epsilon value that is greater than 0
                disp("Unbounded");
                break
            end
        
            fprintf('x%d is entering the basis and x%d is leaving\n', j, basic1(i));
            basic1(i) = j;
            pivot_element = A1(i, j);

            %Compute row operations

            A1(i, :) = A1(i, :)./pivot_element;  %Compute the pivot row first
            b1(i, :) = b1(i, :) ./ pivot_element;

            for index = 1:m1 %update Each row in A1
                if index == i  %Already updated pivot row
                    continue
                end
                ratio = A1(index, j)/A1(i,j);
                A1(index, :) = A1(index, :) - ratio .* A1(i,:);
                b1(index, :) = b1(index, :) - ratio .* b1(i, :);
            end

        %Update r
        ratio = r1(1, j)/A1(i,j);
        r1(1, :) = r1(1, :) - ratio .* A1(i,:);
        end
        
        %Find Current X and Z
        X = zeros(n1, 1);
        for index = 1:m1
            X(basic1(index)) = b1(index);
        end 

        %Display X
        fprintf("\nCurrent X:\n")
        fprintf(fileID, "\nCurrent X:\n");
        for index=1:n1
            if index <= on
                fprintf("X%d = %d, ", index, X(index));
                fprintf(fileID, "X%d = %d, ", index, X(index));
            end
        end
  

    end
    

    %Slice the Matrices to remove the Artificial Variables and pass them on
    %to Phase II
    basic = basic1;
    A = A1(1:m, 1:n);
    b = b1;
    r  = C;
    
    
    
    X = zeros(n, 1);
    for index = 1:m
        X(basic(index)) = b(index);
    end
    
    feasible = 1;
    for i=1:om
        if type == 1
            sX = X(1:on);
            LHS = oA(i, :) * sX;
            if LHS ~= ob(i)
                feasible = 0;
            end
        elseif type == 2
            sX = X(1:on);
            LHS = oA(i, :) * sX;
            if LHS > ob(i)
                feasible = 0;
            end
        else
            sX = X(1:on);
            LHS = oA(i, :) * sX;
            if LHS < ob(i)
                feasible = 0;
            end
        end
    end
    
    if feasible == 0
        fprintf('\n\nThe Original Problem is infeasible (Constraint Violated)');
        fprintf(fileID, '\n\nThe Orginal problem is infeasible (Constraint Violated)');
        Elapsed_time = toc(start);
        fprintf(fileID, "\n\nTime Taken: %f seconds\n", Elapsed_time);
        fprintf("\n\nTime Taken: %f seconds\n", Elapsed_time);
        fclose(fileID);
        clear variables;
        return
    end
    
end






%%%Make r accurate for the basis with Row Operations



fprintf("For Phase II, The basis is initially:\n");
for i = 1:m
    fprintf("x%d ", basic(i));
end
fprintf("\n");


fprintf("\nOur original BFS is ");
fprintf(fileID, "\nOur original BFS is ");
X = zeros(n, 1);
for index = 1:m
    X(basic(index)) = b(index);
end
for index=1:n
    if index <= on
        fprintf("X%d = %d, ", index, X(index));
        fprintf(fileID, "X%d = %d, ", index, X(index));
    end
end
fprintf('\n');
fprintf(fileID, '\n');

%Remove Unwanted Variables
clear i j column I;

%Make r = 0 for basic variables


for i=1:m
        ratio = r(basic(i));
        r = r - ratio.*A(i, :);
end


fprintf(fileID, '\nPhase II started.\n');


FoundOptimal = 1;   %Determines if Unbounded

iteration = 0;


%Phase 2 -- Assumes that we already have a B.F.S
while 1  %Each iteration corresponds to one step
    
   
    %Find pivot column
    [minimum, j] = min(r);
    if minimum >= 0         %No negative r
        fprintf("\n\nOptimal Solution Reached.");
        fprintf(fileID, "\n\nOptimal Solution Reached.");
        break
    else %Find pivot row, check column j
        iteration = iteration +1 ;
        fprintf("\nPhase II Iteration %d\n", iteration);
        fprintf(fileID, "\nPhase II Iteration %d\n", iteration);
        ratio_column = zeros(m, 1);
        
        for i=1:m
            ratio_column(i) = b(i) / A(i,j);
            if (A(i,j) <= 0) || (ratio_column(i) <= 0)
                ratio_column(i) = 1/0;   %If negative value of b or ratio then set to infinity
            end
        end
        
        [minimum, i] = min(ratio_column);
        if minimum == Inf      %No epsilon value that is greater than 0
            disp("Unbounded");
            FoundOptimal = 0;
            break
        end
        
        fprintf('x%d is entering the basis and x%d is leaving\n', j, basic(i));
        basic(i) = j;
        pivot_element = A(i, j);

        %Compute row operations

        A(i, :) = A(i, :)./pivot_element;  %Compute the pivot row first
        b(i, :) = b(i, :) ./ pivot_element;

        for index = 1:m %update Each row in A
            if index == i  %Already updated pivot row
                continue
            end
            ratio = A(index, j)/A(i,j);
            A(index, :) = A(index, :) - ratio .* A(i,:);
            b(index, :) = b(index, :) - ratio .* b(i, :);
        end
    
    %Update r
    ratio = r(1, j)/A(i,j);
    r(1, :) = r(1, :) - ratio .* A(i,:);
    
    end
    
    %Find Current X and Z
    X = zeros(n, 1);
    for index = 1:m
        X(basic(index)) = b(index);
    end

    Z = C*X;  
    
    %Display X and Z
    fprintf("\nCurrent X:\n");
    fprintf(fileID, "\nCurrent X:\n");
    for index=1:n
        if index <= on
            fprintf("X%d = %d, ", index, X(index));
            fprintf(fileID, "X%d = %d, ", index, X(index));
        end
    end
    
    if maxS == 1
        fprintf("Current Z = %d\n", -Z + Constant);
        fprintf(fileID, "Current Z = %d\n", -Z +Constant);
    else
        fprintf("Current Z = %d\n", Z + Constant);
        fprintf(fileID, "Current Z = %d\n", Z +Constant);
    end
end


X = zeros(n, 1);
for index = 1:m
    X(basic(index)) = b(index);
end

%Display Optimal X and Z
if FoundOptimal == 1     %Optimal
    fprintf("\nOptimal X:\n");
    fprintf(fileID, "\nOptimal X:\n");
    for index=1:n
        if index <= on
            fprintf("X%d = %d, ", index, X(index));
            fprintf(fileID, "X%d = %d, ", index, X(index));
        end
    end
    
    if maxS == 1
        fprintf("Maximum Z = %d\n", -(C*X) + Constant);
        fprintf(fileID, "Maximum Z = %d\n", -(C*X) + Constant);
    else
        fprintf("Minimum Z = %d\n", C*X + Constant);
        fprintf(fileID, "Minimum Z = %d\n", C*X + Constant);
    end
    
    %Check if X* violates constraints
    
    feasible = 1;
    for i=1:om
        if type == 1
            fprintf('\n\nFor Constraint %d:', i);
            fprintf(fileID, '\n\nFor Constraint %d:', i);
            sX = X(1:on);
            LHS = oA(i, :) * sX;
            fprintf('\nLHS = %f = %f = b(%d), ', LHS, ob(i), i);
            fprintf(fileID, '\nLHS = %f = %f = b(%d), ', LHS, ob(i), i);
            if LHS == ob(i)
                fprintf('Correct.');
                fprintf(fileID, 'Correct.');
            else
                feasible = 0;
                fprintf('Wrong.');
                fprintf(fileID, 'Wrong.');
            end
        elseif type == 2
            fprintf('\n\nFor Constraint %d:', i);
            fprintf(fileID, '\n\nFor Constraint %d:', i);
            sX = X(1:on);
            LHS = oA(i, :) * sX;
          
            fprintf('\nLHS = %f <= %f = b(%d), ', LHS, ob(i), i);
            fprintf(fileID, '\nLHS = %f <= %f = b(%d), ', LHS, ob(i), i);
            if LHS <= ob(i)
                fprintf('Correct.');
                fprintf(fileID, 'Correct.');
            else
                feasible = 0;
                fprintf('Wrong.');
                fprintf(fileID, 'Wrong.');
            end
        else
            fprintf('\n\nFor Constraint %d:', i);
            fprintf(fileID, '\n\nFor Constraint %d:', i);
            sX = X(1:on);
            LHS = oA(i, :) * sX;
            fprintf('\nLHS = %f >= %f = b(%d), ', LHS, ob(i), i);
            fprintf(fileID, '\nLHS = %f >= %f = b(%d), ', LHS, ob(i), i);
            if LHS >= ob(i)
                fprintf('Correct.');
                fprintf(fileID, 'Correct.');
            else
                feasible = 0;
                fprintf('Wrong.');
                fprintf(fileID, 'Wrong.');
            end
        end
    end
    
    sX = X(1:on);
    minimum = min(sX);
    if feasible == 0
        fprintf(fileID, '\n\nThe solution is infeasible (Constraint Violated)');
        fprintf('\n\nThe solution is infeasible (Constraint Violated)');
    end
    
    if minimum >= 0 && feasible == 1
        fprintf(fileID, '\n\nThe solution is feasible (all Xi >=0)');
        fprintf('\n\nThe solution is feasible (all Xi >=0)');
    end
    if minimum < 0 && feasible == 1
        fprintf(fileID, '\nThe solution is infeasible (not all Xi >=0)');
        fprintf('\n\nThe solution is infeasible (not all Xi >=0)');
    end
    
    fprintf('\n');
    fprintf(fileID, '\n');
        
    
else %Unbounded
    if maxS == 1
        fprintf("\nProblem is Unbounded. Maximum Z is Infinity\n");
        fprintf(fileID, "\nProblem is Unbounded. Maximum Z is Infinity\n");
    else
        fprintf("\nProblem is Unbounded. Minimum Z is -Infinity\n");
        fprintf(fileID, "\nProblem is Unbounded. Minimum Z is -Infinity\n");
    end
end

Elapsed_time = toc(start);
fprintf(fileID, "\n\nTime Taken: %f seconds\n", Elapsed_time);
fprintf("\n\nTime Taken: %f seconds\n", Elapsed_time);
fclose(fileID);
clear variables;





