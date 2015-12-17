
M = csvread('C:\cygwin64\home\Albin\patternRecognitionP1\mnist.csv', 1, 0 );


%%
for i=0:9
    M1 = M(ismember(M(:,1), [i]),:);

    M1avg = mean(M1(:,:));
    M1avg = (reshape(M1avg(2:end),28,28)');
    
    imagesc(M1avg)
    colormap(gray)
    
    %M1avg=M1avg/255
    %imwrite(M1avg, 'C:\cygwin64\home\Albin\patternRecognitionP1\im1.png');
    
    waitforbuttonpress
end

%%
M = csvread('C:\cygwin64\home\Albin\patternRecognitionP1\mean_stdev.csv', 1, 1 );
%%
mean = (reshape(M(1, :),28,28)');
imagesc(mean)
colormap(gray)
waitforbuttonpress
stdev = (reshape(M(2, :),28,28)');
imagesc(stdev)
colormap(gray)