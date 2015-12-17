
M = csvread('C:\cygwin64\home\Albin\patternRecognitionP1\mnist.csv', 1, 0 );
newsize=14;
M2 = zeros(size(M,1), newsize^2);

%%
for i=1:size(M,1)
    M(i,1)
    test=(reshape(M(i,2:end),28,28)');
    test2=imresize(test, [newsize newsize]);
    test2(test2<0)=0;
    M2(i, :)=test2(:);
    imagesc(test2)
    waitforbuttonpress;
end

%csvwrite('C:\cygwin64\home\Albin\patternRecognitionP1\mnist2.csv', M2);
