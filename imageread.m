
M = csvread('C:\cygwin64\home\Albin\patternRecognitionP1\mnist.csv', 1, 0 ); %, [1 0 3000 785]);



M= M(1:3000, :);
M1 = M(ismember(M(:,1), [1]),:);

M1avg = mean(M1(:,:));
M1avg = (reshape(M1avg(2:end),28,28)');
%test = reshape(M1, 28, 28)';

%%
for i=1:20 
M(i,1)
test=(reshape(M(i,2:end),28,28)');
%test(test<128) = 0;
test2 = abs(test - M1avg);
subplot(3,1,1)
imagesc(M1avg)
subplot(3,1,2)
imagesc(test)
subplot(3,1,3)
imagesc(test2)
colormap(gray)

sum(sum(test2))

k=waitforbuttonpress;
end
